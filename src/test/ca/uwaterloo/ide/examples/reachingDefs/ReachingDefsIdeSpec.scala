package ca.uwaterloo.ide.examples.reachingDefs

import java.io.ByteArrayInputStream
import java.lang.Iterable

import ca.uwaterloo.ide.conversion.{IdeFromIfdsBuilder, IdeResultToIfdsResult}
import ca.uwaterloo.ide.problem.solver.{PartiallyBalancedPropagator, IdeSolver}
import com.ibm.wala.core.tests.callGraph.CallGraphTestUtil
import com.ibm.wala.core.tests.util.TestConstants
import com.ibm.wala.dataflow.IFDS.{TabulationProblem, TabulationSolver}
import com.ibm.wala.examples.analysis.dataflow.DataflowTest
import com.ibm.wala.ipa.callgraph._
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.cfg.BasicBlockInContext
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.ssa.analysis.IExplodedBasicBlock
import com.ibm.wala.util.collections.Pair
import com.ibm.wala.util.config.{AnalysisScopeReader, FileOfClasses}

import scala.collection.JavaConverters._

object ReachingDefsIdeSpec {

  def main(args: Array[String]): Unit = {
    val problem         = new ReachingDefsIdeProblem(cg) with IdeResultToIfdsResult
    val result          = problem.ideResultToIfdsResult
    val ideNodesReached = result.getSupergraphNodesReached.asScala.toSet

    println("IDE size: " + ideNodesReached.size)

    val originalIfdsSolver = TabulationSolver.make(new ReachingDefsProblem(cg, new AnalysisCache))
    val originalResult     = originalIfdsSolver.solve
    val ifdsNodesReached   = originalResult.getSupergraphNodesReached.asScala.toSet

    println("IFDS size: " + ifdsNodesReached.size)

    println("\ncontained in IFDS but not in IDE:")
    val ifdsNotIde = ifdsNodesReached diff ideNodesReached
    ifdsNotIde foreach {
      n =>
        println(n)
    }

    println("\ncontained in IDE but not in IFDS:")
    ideNodesReached diff ifdsNodesReached foreach {
      n =>
        println(n)
    }
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
    val entrypoints: Iterable[Entrypoint] = com.ibm.wala.ipa.callgraph.impl.Util.makeMainEntrypoints(scope, cha, "Ldataflow/Simple")
    val options: AnalysisOptions = CallGraphTestUtil.makeAnalysisOptions(scope, entrypoints)
    val builder: CallGraphBuilder = Util.makeZeroOneCFABuilder(options, new AnalysisCache, cha, scope)
    builder.makeCallGraph(options, null)
  }
}

class ReachingDefsIdeProblem(cg: CallGraph) extends IdeFromIfdsBuilder with IdeSolver with PartiallyBalancedPropagator {

  override type F = Pair[CGNode, Integer]
  override type Node = BasicBlockInContext[IExplodedBasicBlock]
  override type Procedure = CGNode
  override val walaIfdsProblem: TabulationProblem[Node, Procedure, F] = new ReachingDefsProblem(cg, new AnalysisCache)
}
