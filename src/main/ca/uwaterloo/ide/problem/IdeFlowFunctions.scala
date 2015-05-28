package ca.uwaterloo.ide.problem

import ca.ide.types.LabeledExplodedGraphTypes
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
  def callStartEdges(node: XNode, tpe: NodeType): Set[FactFunPair]

  /**
   * Functions for intra-procedural edges from a call to the corresponding return edges.
   */
  def callReturnEdges(node: XNode, tpe: NodeType): Set[FactFunPair]

  /**
   * Functions for inter-procedural edges from an end node to the return node of the callee function.
   */
  def endReturnEdges(node: XNode, tpe: NodeType): Set[FactFunPair]

  /**
   * Functions for all other (inter-procedural) edges.
   */
  def otherSuccEdges(node: XNode): Set[FactFunPair]

  /**
   * Functions for phi instructions.
   */
  def otherSuccEdgesPhi(node: XNode): Set[FactFunPair]

  /**
   * Helper function analogous to callStartFns, but returns only the factoids, without the edge functions.
   */
  def callStartD2s: (XNode, NodeType) => Set[Fact] =
    (node1, n2) =>
      callStartEdges(node1, n2) map { _.d2 }

  final val idFactFunPairSet = (d: Fact) => Set(FactFunPair(d, Id))
}
