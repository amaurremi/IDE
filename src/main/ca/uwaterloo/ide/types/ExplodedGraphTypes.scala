package ca.uwaterloo.ide.types

trait ExplodedGraphTypes extends SuperGraphTypes {

  /**
   * The type for propagated facts (corresponds to elements of the set D)
   */
  type Fact

  /**
   * Representation of the Λ (zero) fact
   */
  val Λ: Fact

  /**
   * A node in the exploded supergraph
   */
  sealed trait XNode {
    val n: Node
    val d: Fact
    val isStartNode: Boolean
    val isExitNode: Boolean
    val isReturnNode: Boolean
    val isCallNode: Boolean

    override def equals(obj: scala.Any): Boolean =
      obj match {
        case node: XNode => node.n == n && node.d == d
        case _           => false
      }

    override def hashCode: Int =
      41 * (41 + n.hashCode) + d.hashCode

    override def toString: String = "IdeNode(" + n.toString + ", " + d.toString + ")"
  }

  object XNode {
    def apply(node: Node, fact: Fact): XNode =
      new XNode {
        override val n = node
        override val d = fact
        override lazy val isStartNode  = supergraph isEntry node
        override lazy val isReturnNode = supergraph isReturn node
        override lazy val isExitNode   = supergraph isExit node
        override lazy val isCallNode   = supergraph isCall node
      }

    def unapply(node: XNode): Option[(Node, Fact)] = Some(node.n, node.d)
  }

  /**
   * An edge in the exploded supergraph
   */
  case class XEdge(source: XNode, target: XNode)
}
