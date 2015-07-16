package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.types.ExplodedGraphTypes
import com.ibm.wala.util.Predicate
import com.ibm.wala.util.graph.traverse.DFS

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.mutable

trait TraverseGraph { this: ExplodedGraphTypes =>

  private[this] val followingNodesCache = mutable.Map[Node, Iterable[Node]]()

//  private[this] val enclProcCache = mutable.Map[Node, Procedure]()

//  private[this] val startNodeCache = mutable.Map[Procedure, Iterable[Node]]()

  def followingNodes(n: Node): Iterable[Node] =
    followingNodesCache.getOrElseUpdate(n, (supergraph getSuccNodes n).toIterable)

  /**
   * Returns the enclosing procedure of a given node.
   */
  def enclProc(node: Node): Procedure =
//    enclProcCache getOrElseUpdate (node, supergraph.getProcOf(node))
    supergraph getProcOf node

  /**
   * Given a call node n, returns the start nodes of n's target procedures.
   */
  def targetStartNodes(n: Node): Iterable[Node] =
    (supergraph getCalledNodes n).toIterable

  /**
   * Return-site nodes that correspond to call node n to target start node s
   */
  def returnNodes(n: Node, s: Option[Node]): Iterable[Node] = {
    val proc = s match {
      case Some(sn) => enclProc(sn)
      case None     => null.asInstanceOf[Procedure]
    }
    supergraph.getReturnSites(n, proc).toIterable
  }

  /**
   * Returns the start node of the node's enclosing procedure.
   */
  def startNodes(p: Procedure): Iterable[Node] =
//    startNodeCache getOrElseUpdate (p, (supergraph getEntriesForProcedure p).toIterable)
    (supergraph getEntriesForProcedure p).toIterable

  /**
   * Given the exit node of procedure p, returns all pairs (c, r), where c calls p with corresponding
   * return-site node r.
   */
  def callReturnPairs(exit: Node): Iterable[(Node, Node)] = // todo is that right?
    for {
      r <- followingNodes(exit)
      rn = r
      c <- getCallSites(rn, enclProc(exit))
      if followingNodes(c) contains rn
    } yield c -> r

  def getCallSites(node: Node, proc: Procedure): Iterable[Node] =
    supergraph.getCallSites(node, proc).toIterable

  /**
   * All call nodes inside of a given procedure
   */
  def callNodesInProc(p: Procedure): Iterable[Node] = {
    val nodesInProc = DFS.getReachableNodes(
      supergraph,
      startNodes(p),
      new Predicate[Node]() {
        override def test(n: Node): Boolean = enclProc(n) == p
      }
    )
    nodesInProc filter supergraph.isCall
  }

  def traverseSupergraph = supergraph.iterator.asScala
}
