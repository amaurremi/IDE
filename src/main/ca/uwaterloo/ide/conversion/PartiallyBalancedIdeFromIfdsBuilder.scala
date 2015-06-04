package ca.uwaterloo.ide.conversion

import ca.uwaterloo.ide.problem.PartiallyBalancedIdeFlowFunctions
import ca.uwaterloo.ide.problem.solver.PartiallyBalancedPropagator
import com.ibm.wala.dataflow.IFDS.{IUnaryFlowFunction, TabulationProblem, IPartiallyBalancedFlowFunctions, PartiallyBalancedTabulationProblem}

trait PartiallyBalancedIdeFromIfdsBuilder
  extends IdeFromIfdsBuilder
  with PartiallyBalancedIdeFlowFunctions
  with PartiallyBalancedPropagator
{

  override def walaIfdsProblem: PartiallyBalancedTabulationProblem[Node, Procedure, F]

  override def fakeEntry(n: Node) = walaIfdsProblem.getFakeEntry(n)

  def unbalancedReturnFlowFunction(src: XNode, dest: Node): Iterable[FactFunPair] =
    walaIfdsProblem.getFunctionMap.getUnbalancedReturnFlowFunction(src.n, dest) match {
      case f: IUnaryFlowFunction =>
        zipWithId(unaryIterator(f, src))
      case _                     =>
        throw new UnsupportedOperationException("unbalanced return flow function should always return a unary flow function")
    }
}
