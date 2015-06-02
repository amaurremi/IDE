package ca.uwaterloo.ide.examples

import java.io.ByteArrayInputStream

import ca.uwaterloo.ide.conversion.{IdeResultToIfdsResult, IdeFromIfdsBuilder}
import ca.uwaterloo.ide.problem.solver.IdeSolver
import ca.uwaterloo.ide.util.Time
import com.ibm.wala.core.tests.callGraph.CallGraphTestUtil
import com.ibm.wala.core.tests.util.TestConstants
import com.ibm.wala.dataflow.IFDS.{PartiallyBalancedTabulationSolver, TabulationProblem}
import com.ibm.wala.examples.analysis.dataflow.DataflowTest
import com.ibm.wala.ipa.callgraph._
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.cfg.BasicBlockInContext
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.ssa.analysis.IExplodedBasicBlock
import com.ibm.wala.util.collections.Pair
import com.ibm.wala.util.config.{AnalysisScopeReader, FileOfClasses}

object ReachingDefsIdeSpec {

  def main(args: Array[String]): Unit = {
    val problem = new ReachingDefsIdeProblem(cg) with IdeResultToIfdsResult
    val result = Time.time("compute result") {
      problem.ideResultToIfdsResult
    }
    val originalIfdsSolver = PartiallyBalancedTabulationSolver.createPartiallyBalancedTabulationSolver(new ReachingDefsProblem(cg, new AnalysisCache), null)
    val originalResult = Time.time("original IFDS result") {
      originalIfdsSolver.solve
    }

    val mySize = result.getSupergraphNodesReached.size()
    val originalSize = originalResult.getSupergraphNodesReached.size

    printf("my size: %s\noriginal size: %s\n", mySize, originalSize)
  }

  private[this] val cg = {
    val scope = AnalysisScopeReader.readJavaScope(TestConstants.WALA_TESTDATA, null, classOf[DataflowTest].getClassLoader)
    val exclusions =
      "java\\/awt\\/.*\n" +
        "javax\\/swing\\/.*\n" +
        "sun\\/awt\\/.*\n" +
        "sun\\/swing\\/.*\n" +
        "com\\/sun\\/.*\n" +
        "sun\\/.*\n" +
        "org\\/netbeans\\/.*\n" +
        "org\\/openide\\/.*\n" +
        "com\\/ibm\\/crypto\\/.*\n" +
        "com\\/ibm\\/security\\/.*\n" +
        "org\\/apache\\/xerces\\/.*\n" +
        "java\\/security\\/.*\n"
    scope.setExclusions(new FileOfClasses(new ByteArrayInputStream(exclusions.getBytes("UTF-8"))))
    val cha = ClassHierarchy.make(scope)
    val entrypoints = com.ibm.wala.ipa.callgraph.impl.Util.makeMainEntrypoints(scope, cha, "Ldataflow/StaticDataflow")
    val options: AnalysisOptions = CallGraphTestUtil.makeAnalysisOptions(scope, entrypoints)
    val builder: CallGraphBuilder = Util.makeZeroOneCFABuilder(options, new AnalysisCache, cha, scope)
    builder.makeCallGraph(options, null)
  }
}

class ReachingDefsIdeProblem(cg: CallGraph) extends IdeFromIfdsBuilder with IdeSolver {

  override type F = Pair[CGNode, Integer]
  override type Node = BasicBlockInContext[IExplodedBasicBlock]
  override type Procedure = CGNode
  override val walaIfdsProblem: TabulationProblem[Node, Procedure, F]
    = Time.time("reaching defs problem") { new ReachingDefsProblem(cg, new AnalysisCache) }
}
