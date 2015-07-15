package ca.uwaterloo.ide.conversion

import ca.uwaterloo.ide.problem.PartiallyBalancedIdeFlowFunctions
import ca.uwaterloo.ide.problem.solver.{IdeSolver, PartiallyBalancedPropagator}
import com.ibm.wala.dataflow.IFDS.{IUnaryFlowFunction, PartiallyBalancedTabulationProblem}

trait PartiallyBalancedIdeFromIfdsBuilder
  extends IdeFromIfdsBuilder
  with PartiallyBalancedIdeFlowFunctions
  with PartiallyBalancedPropagator
  with IdeResultToIfdsResult
  with IdeSolver
{

  override def walaIfdsProblem: PartiallyBalancedTabulationProblem[Node, Procedure, F]

  override def fakeEntry(n: Node) = walaIfdsProblem.getFakeEntry(n)

  def unbalancedReturnFlowFunction(src: XNode, dest: Node): Iterator[FactFunPair] =
    walaIfdsProblem.getFunctionMap.getUnbalancedReturnFlowFunction(src.n, dest) match {
      case f: IUnaryFlowFunction =>
        zipWithId(unaryIterator(f, src))
      case _                     =>
        throw new UnsupportedOperationException("unbalanced return flow function should always return a unary flow function")
    }
}
