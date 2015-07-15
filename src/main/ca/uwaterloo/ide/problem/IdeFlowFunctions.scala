package ca.uwaterloo.ide.problem

import ca.uwaterloo.ide.problem.types.LabeledExplodedGraphTypes

/**
 * Defines functions that, for an edge (n, d1) -> (m, d2),
 * for which we know n, d1, and m, return all
 * d2s plus the corresponding edge IDE functions.
 */
trait IdeFlowFunctions extends LabeledExplodedGraphTypes {

  case class FactFunPair(
    d2: Fact,
    edgeFn: MicroFunction
  )

  // todo documentation
  sealed trait ReturnFlowFunctionType
  case class BinaryReturnFlowFunction(callFact: Fact => Iterator[FactFunPair]) extends ReturnFlowFunctionType
  case class UnaryReturnFlowFunction(pairs: Iterator[FactFunPair]) extends ReturnFlowFunctionType

  /**
   * Functions for inter-procedural edges from a call node to the corresponding start edges.
   */
  def callFlowFunction(src: XNode, dest: Node, ret: Node): Iterator[FactFunPair]

  /**
   * the flow function for a call-return edge, when the supergraph does not contain any callees of src
   */
  def callNoneToReturnFlowFunction(src: XNode, dest: Node): Iterator[FactFunPair]

  /**
   * Functions for intra-procedural edges from a call to the corresponding return edges.
   */
  def callToReturnFlowFunction(src: XNode, dest: Node): Iterator[FactFunPair]

  /**
   * Functions for inter-procedural edges from an end node to the return node of the callee function.
   */
  def returnFlowFunction(callN: Node, src: XNode, dest: Node): ReturnFlowFunctionType

  /**
   * Functions for all other (inter-procedural) edges.
   */
  def normalFlowFunction(src: XNode, dest: Node): Iterator[FactFunPair]

  /**
   * Helper function analogous to callStartFns, but returns only the factoids, without the edge functions.
   */
  def callStartD2s(node1: XNode, n2: Node, ret: Node): Iterator[Fact] =
      callFlowFunction(node1, n2, ret) map { _.d2 }

  final val idFactFunPairSet = (d: Fact) => Set(FactFunPair(d, Id))
}

trait PartiallyBalancedIdeFlowFunctions extends IdeFlowFunctions {
  
  /**
   * This version should work when the "call" instruction was never reached normally. This applies only when using partially
   * balanced parentheses.
   */
  def unbalancedReturnFlowFunction(src: XNode, dest: Node): Iterator[FactFunPair]

  /**
   * The entry node that should be used as the start node of the jump function to n. This node
   * must be the entry node of the procedure containing n
   */
  def fakeEntry(n: Node): Node
}