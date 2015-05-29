package ca.uwaterloo.ide.problem

import ca.uwaterloo.ide.types.LabeledExplodedGraphTypes

/**
 * Defines functions that, for an edge (n, d1) -> (m, d2),
 * for which we know n, d1, and m, return all
 * d2s plus the corresponding edge IDE functions.
 */
trait IdeFlowFunctions { this: LabeledExplodedGraphTypes =>

  case class FactFunPair(
    d2: Fact,
    edgeFn: IdeFunction
  )

  /**
   * Functions for inter-procedural edges from a call node to the corresponding start edges.
   */
  def callFlowFunction(node: XNode, tpe: NodeType): Iterable[FactFunPair]

  /**
   * Functions for intra-procedural edges from a call to the corresponding return edges.
   */
  def callToReturnFlowFunction(node: XNode, tpe: NodeType): Iterable[FactFunPair]

  /**
   * Functions for inter-procedural edges from an end node to the return node of the callee function.
   */
  def returnFlowFunction(call: NodeType, node: XNode, tpe: NodeType): Iterable[FactFunPair]

  /**
   * Functions for all other (inter-procedural) edges.
   */
  def normalFlowFunction(node: XNode, tpe: NodeType): Iterable[FactFunPair]

  /**
   * Functions for phi instructions.
   */
  def normalPhiFlowFunction(node: XNode, tpe: NodeType): Iterable[FactFunPair]

  /**
   * Helper function analogous to callStartFns, but returns only the factoids, without the edge functions.
   */
  def callStartD2s: (XNode, NodeType) => Iterable[Fact] =
    (node1, n2) =>
      callFlowFunction(node1, n2) map { _.d2 }

  final val idFactFunPairSet = (d: Fact) => Set(FactFunPair(d, Id))
}
