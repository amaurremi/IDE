package ca.uwaterloo.ide.examples.reachingDefs;

import com.ibm.wala.classLoader.IField;
import com.ibm.wala.dataflow.IFDS.*;
import com.ibm.wala.ipa.callgraph.AnalysisCache;
import com.ibm.wala.ipa.callgraph.CGNode;
import com.ibm.wala.ipa.callgraph.CallGraph;
import com.ibm.wala.ipa.cfg.BasicBlockInContext;
import com.ibm.wala.ipa.cha.IClassHierarchy;
import com.ibm.wala.ssa.SSAInstruction;
import com.ibm.wala.ssa.SSAPutInstruction;
import com.ibm.wala.ssa.analysis.IExplodedBasicBlock;
import com.ibm.wala.util.collections.HashSetFactory;
import com.ibm.wala.util.collections.Pair;
import com.ibm.wala.util.intset.IntSet;
import com.ibm.wala.util.intset.MutableMapping;
import com.ibm.wala.util.intset.MutableSparseIntSet;

import java.util.Collection;

/**
 * Slightly modified copy of WALA's ReachingDefsProblem which is a private class in ContextSensitiveReachingDefs
 */
public class ReachingDefsProblem implements
        PartiallyBalancedTabulationProblem<BasicBlockInContext<IExplodedBasicBlock>, CGNode, Pair<CGNode, Integer>> {

    private final ISupergraph<BasicBlockInContext<IExplodedBasicBlock>, CGNode> supergraph;
    private final ReachingDefsDomain domain = new ReachingDefsDomain();

    private final IClassHierarchy cha;
    private final ReachingDefsFlowFunctions flowFunctions = new ReachingDefsFlowFunctions(domain);
    private final Collection<PathEdge<BasicBlockInContext<IExplodedBasicBlock>>> initialSeeds;

    public ReachingDefsProblem(CallGraph cg, AnalysisCache cache) {
        this.cha =  cg.getClassHierarchy();
        this.supergraph = ICFGSupergraph.make(cg, cache);
        initialSeeds = collectInitialSeeds();
    }

    /**
     * we use the entry block of the CGNode as the fake entry when propagating from callee to caller with unbalanced parens
     */
    @Override
    public BasicBlockInContext<IExplodedBasicBlock> getFakeEntry(BasicBlockInContext<IExplodedBasicBlock> node) {
        final CGNode cgNode = node.getNode();
        return getFakeEntry(cgNode);
    }

    /**
     * we use the entry block of the CGNode as the "fake" entry when propagating from callee to caller with unbalanced parens
     */
    private BasicBlockInContext<IExplodedBasicBlock> getFakeEntry(final CGNode cgNode) {
        BasicBlockInContext<IExplodedBasicBlock>[] entriesForProcedure = supergraph.getEntriesForProcedure(cgNode);
        assert entriesForProcedure.length == 1;
        return entriesForProcedure[0];
    }

    /**
     * collect the putstatic instructions in the call graph as {@link PathEdge} seeds for the analysis
     */
    private Collection<PathEdge<BasicBlockInContext<IExplodedBasicBlock>>> collectInitialSeeds() {
        Collection<PathEdge<BasicBlockInContext<IExplodedBasicBlock>>> result = HashSetFactory.make();
        for (BasicBlockInContext<IExplodedBasicBlock> bb : supergraph) {
            IExplodedBasicBlock ebb = bb.getDelegate();
            SSAInstruction instruction = ebb.getInstruction();
            if (instruction instanceof SSAPutInstruction && bb.getMethod().getName().toString().contains("main")) {
                SSAPutInstruction putInstr = (SSAPutInstruction) instruction;
                if (putInstr.isStatic()) {
                    final CGNode cgNode = bb.getNode();
                    Pair<CGNode, Integer> fact = Pair.make(cgNode, ebb.getFirstInstructionIndex());
                    int factNum = domain.add(fact);
                    BasicBlockInContext<IExplodedBasicBlock> fakeEntry = getFakeEntry(cgNode);
                    // note that the fact number used for the source of this path edge doesn't really matter
                    result.add(PathEdge.createPathEdge(fakeEntry, factNum, bb, factNum));

                }
            }
        }
        return result;
    }

    @Override
    public IPartiallyBalancedFlowFunctions<BasicBlockInContext<IExplodedBasicBlock>> getFunctionMap() {
        return flowFunctions;
    }

    @Override
    public TabulationDomain<Pair<CGNode, Integer>, BasicBlockInContext<IExplodedBasicBlock>> getDomain() {
        return domain;
    }

    /**
     * we don't need a merge function; the default unioning of tabulation works fine
     */
    @Override
    public IMergeFunction getMergeFunction() {
        return null;
    }

    @Override
    public ISupergraph<BasicBlockInContext<IExplodedBasicBlock>, CGNode> getSupergraph() {
        return supergraph;
    }

    @Override
    public Collection<PathEdge<BasicBlockInContext<IExplodedBasicBlock>>> initialSeeds() {
        return initialSeeds;
    }

    private class ReachingDefsFlowFunctions implements IPartiallyBalancedFlowFunctions<BasicBlockInContext<IExplodedBasicBlock>> {

        private final ReachingDefsDomain domain;

        protected ReachingDefsFlowFunctions(ReachingDefsDomain domain) {
            this.domain = domain;
        }

        /**
         * the flow function for flow from a callee to caller where there was no flow from caller to callee; just the identity function
         *
         * @see ReachingDefsProblem
         */
        @Override
        public IFlowFunction getUnbalancedReturnFlowFunction(BasicBlockInContext<IExplodedBasicBlock> src,
                                                             BasicBlockInContext<IExplodedBasicBlock> dest) {
            return IdentityFlowFunction.identity();
        }

        /**
         * flow function from caller to callee; just the identity function
         */
        @Override
        public IUnaryFlowFunction getCallFlowFunction(BasicBlockInContext<IExplodedBasicBlock> src,
                                                      BasicBlockInContext<IExplodedBasicBlock> dest, BasicBlockInContext<IExplodedBasicBlock> ret) {
            return IdentityFlowFunction.identity();
        }

        /**
         * flow function from call node to return node when there are no targets for the call site; not a case we are expecting
         */
        @Override
        public IUnaryFlowFunction getCallNoneToReturnFlowFunction(BasicBlockInContext<IExplodedBasicBlock> src,
                                                                  BasicBlockInContext<IExplodedBasicBlock> dest) {
            // if we're missing callees, just keep what information we have
            return IdentityFlowFunction.identity();
        }

        /**
         * flow function from call node to return node at a call site when callees exist. We kill everything; surviving facts should
         * flow out of the callee
         */
        @Override
        public IUnaryFlowFunction getCallToReturnFlowFunction(BasicBlockInContext<IExplodedBasicBlock> src,
                                                              BasicBlockInContext<IExplodedBasicBlock> dest) {
            return KillEverything.singleton();
        }

        /**
         * flow function for normal intraprocedural edges
         */
        @Override
        public IUnaryFlowFunction getNormalFlowFunction(final BasicBlockInContext<IExplodedBasicBlock> src,
                                                        BasicBlockInContext<IExplodedBasicBlock> dest) {
            final IExplodedBasicBlock ebb = src.getDelegate();
            SSAInstruction instruction = ebb.getInstruction();
            if (instruction instanceof SSAPutInstruction) {
                final SSAPutInstruction putInstr = (SSAPutInstruction) instruction;
                if (putInstr.isStatic()) {
                    return new IUnaryFlowFunction() {

                        @Override
                        public IntSet getTargets(int d1) {
                            // first, gen this statement
                            int factNum = domain.getMappedIndex(Pair.make(src.getNode(), ebb.getFirstInstructionIndex()));
                            assert factNum != -1;
                            MutableSparseIntSet result = MutableSparseIntSet.makeEmpty();
                            result.add(factNum);
                            // if incoming statement is some different statement that defs the same static field, kill it; otherwise, keep it
                            if (d1 != factNum) {
                                IField staticField = cha.resolveField(putInstr.getDeclaredField());
                                assert staticField != null;
                                Pair<CGNode, Integer> otherPutInstrAndNode = domain.getMappedObject(d1);
                                SSAPutInstruction otherPutInstr = (SSAPutInstruction) otherPutInstrAndNode.fst.getIR().getInstructions()[otherPutInstrAndNode.snd];
                                IField otherStaticField = cha.resolveField(otherPutInstr.getDeclaredField());
                                if (!staticField.equals(otherStaticField)) {
                                    result.add(d1);
                                }
                            }
                            return result;
                        }

                        @Override
                        public String toString() {
                            return "Reaching Defs Normal Flow";
                        }
                    };
                }
            }
            // identity function when src block isn't for a putstatic
            return IdentityFlowFunction.identity();
        }

        /**
         * standard flow function from callee to caller; just identity
         */
        @Override
        public IFlowFunction getReturnFlowFunction(BasicBlockInContext<IExplodedBasicBlock> call,
                                                   BasicBlockInContext<IExplodedBasicBlock> src, BasicBlockInContext<IExplodedBasicBlock> dest) {
            return IdentityFlowFunction.identity();
        }

    }

    private class ReachingDefsDomain extends MutableMapping<Pair<CGNode, Integer>> implements
            TabulationDomain<Pair<CGNode, Integer>, BasicBlockInContext<IExplodedBasicBlock>> {

        @Override
        public boolean hasPriorityOver(PathEdge<BasicBlockInContext<IExplodedBasicBlock>> p1,
                                       PathEdge<BasicBlockInContext<IExplodedBasicBlock>> p2) {
            // don't worry about worklist priorities
            return false;
        }

    }
}
