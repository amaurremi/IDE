package ca.uwaterloo.idea.examples

import ca.uwaterloo.ide.conversion.IdeFromIfdsBuilder
import ca.uwaterloo.ide.solver.IdeSolver
import com.ibm.wala.core.tests.callGraph.CallGraphTestUtil
import com.ibm.wala.core.tests.util.TestConstants
import com.ibm.wala.dataflow.IFDS.PartiallyBalancedTabulationProblem
import com.ibm.wala.examples.analysis.dataflow.DataflowTest
import com.ibm.wala.ipa.callgraph._
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.cfg.BasicBlockInContext
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.ssa.analysis.IExplodedBasicBlock
import com.ibm.wala.util.config.AnalysisScopeReader

object ReachingDefsIdeSpec {

  def main(args: Array[String]): Unit = {
    val problem = new ReachingDefsIdeProblem
    val result = problem.solvedResult
    print("yay")
  }
}

class ReachingDefsIdeProblem extends IdeFromIfdsBuilder with IdeSolver {

  private[this] val scope = AnalysisScopeReader.readJavaScope(TestConstants.WALA_TESTDATA, null, classOf[DataflowTest].getClassLoader)
  private[this] val cha = ClassHierarchy.make(scope)
  private[this] val entrypoints = com.ibm.wala.ipa.callgraph.impl.Util.makeMainEntrypoints(scope, cha, "Ldataflow/StaticDataflow")
  private[this] val options: AnalysisOptions = CallGraphTestUtil.makeAnalysisOptions(scope, entrypoints)
  private[this] val builder: CallGraphBuilder = Util.makeZeroOneCFABuilder(options, new AnalysisCache, cha, scope)
  private[this] val cg: CallGraph = builder.makeCallGraph(options, null)

  override type F = (CGNode, Integer)
  override val walaIfdsProblem: PartiallyBalancedTabulationProblem[BasicBlockInContext[IExplodedBasicBlock], CGNode, (CGNode, Integer)] = new ReachingDefsProblem(cg, new AnalysisCache)
  override type Node = BasicBlockInContext[IExplodedBasicBlock]
  override type Procedure = CGNode
}

