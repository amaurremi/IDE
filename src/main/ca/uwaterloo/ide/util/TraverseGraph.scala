package ca.uwaterloo.ide.util

import ca.uwaterloo.ide.types.Phis
import com.ibm.wala.util.Predicate
import com.ibm.wala.util.graph.traverse.DFS
import ca.ide.types.Phis
import ca.uwaterloo.ide.types.{ExplodedGraphTypes, Phis}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.mutable

trait TraverseGraph { this: ExplodedGraphTypes with Phis =>

  private[this] val followingNodesCache = mutable.Map[NodeType, Seq[NodeType]]()

  private[this] val enclProcCache = mutable.Map[Node, Procedure]()

  private[this] val startNodeCache = mutable.Map[Node, Seq[NodeType]]()

  def followingNodes(n: NodeType): Seq[NodeType] =
    followingNodesCache.getOrElseUpdate(n,
      n match {
        case PhiNode(node)    =>
          Seq(NormalNode(node))
        case NormalNode(node) =>
          (supergraph getSuccNodes node).asScala.toSeq map createNodeType
      })
  
  def createNodeType(node: Node): NodeType =
    if (phiInstructions(node).isEmpty)
      NormalNode(node)
    else
      PhiNode(node)

  /**
   * Returns the enclosing procedure of a given node.
   */
  def enclProc(node: Node): Procedure =
    enclProcCache.getOrElseUpdate(node, supergraph.getProcOf(node))

  /**
   * Given a call node n, returns the start nodes of n's target procedures.
   */
  def targetStartNodes(n: NodeType): Iterator[NodeType] =
    (supergraph getCalledNodes n.node).asScala map createNodeType

  /**
   * Return-site nodes that correspond to call node n
   */
  def returnNodes(n: NodeType): Iterator[NodeType] =
    targetStartNodes(n) flatMap { s =>
      supergraph.getReturnSites(n.node, enclProc(s.node)).asScala map createNodeType
    }

  /**
   * Returns the start node of the node's enclosing procedure.
   */
  def startNodes(n: Node): Seq[NodeType] =
    startNodeCache.getOrElseUpdate(
      n,
      (supergraph getEntriesForProcedure enclProc(n)).view.toSeq map createNodeType)

  /**
   * Given the exit node of procedure p, returns all pairs (c, r), where c calls p with corresponding
   * return-site node r.
   */
  def callReturnPairs(exit: NodeType): Seq[(NormalNode, NodeType)] =
    for {
      r <- followingNodes(exit)
      rn = r.node
      c <- getCallSites(rn, enclProc(exit.node))
      if (supergraph getSuccNodes c).asScala contains rn
    } yield NormalNode(c) -> r

  def getCallSites(node: Node, proc: Procedure): Iterator[Node] =
    supergraph.getCallSites(node, proc).asScala

  /**
   * All call nodes inside of a given procedure
   */
  def callNodesInProc(p: Procedure): Seq[NormalNode] = {
    val nodesInProc = DFS.getReachableNodes(
      supergraph,
      (supergraph getEntriesForProcedure p).toSeq,
      new Predicate[Node]() {
        override def test(n: Node): Boolean = enclProc(n) == p
      }
    ).toSeq
    nodesInProc collect {
      case nip if supergraph isCall nip =>
        NormalNode(nip)
    }
  }

  def traverseSupergraph = supergraph.iterator.asScala
}
