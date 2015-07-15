package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.types.ExplodedGraphTypes
import com.ibm.wala.util.Predicate
import com.ibm.wala.util.graph.traverse.DFS

import scala.collection.JavaConversions._
import scala.collection.mutable

trait TraverseGraph { this: ExplodedGraphTypes =>

  private[this] val followingNodesCache = mutable.Map[Node, Iterator[Node]]()

  private[this] val enclProcCache = mutable.Map[Node, Procedure]()

  private[this] val startNodeCache = mutable.Map[Node, Iterable[Node]]()

  def followingNodes(n: Node): Iterator[Node] =
    followingNodesCache.getOrElseUpdate(n, supergraph getSuccNodes n)

  /**
   * Returns the enclosing procedure of a given node.
   */
  def enclProc(node: Node): Procedure =
    enclProcCache getOrElseUpdate (node, supergraph.getProcOf(node))

  /**
   * Given a call node n, returns the start nodes of n's target procedures.
   */
  def targetStartNodes(n: Node): Iterator[Node] =
    supergraph getCalledNodes n

  /**
   * Return-site nodes that correspond to call node n to target start node s
   */
  def returnNodes(n: Node, s: Option[Node]): Iterator[Node] = {
    val proc = s match {
      case Some(sn) => enclProc(sn)
      case None     => null.asInstanceOf[Procedure]
    }
    supergraph getReturnSites (n, proc)
  }

  /**
   * Returns the start node of the node's enclosing procedure.
   */
  def startNodes(n: Node): Iterable[Node] =
    startNodeCache.getOrElseUpdate(
      n,
      supergraph getEntriesForProcedure enclProc(n))

  /**
   * Given the exit node of procedure p, returns all pairs (c, r), where c calls p with corresponding
   * return-site node r.
   */
  def callReturnPairs(exit: Node): Iterator[(Node, Node)] = // todo is that right?
    for {
      r <- followingNodes(exit)
      rn = r
      c <- getCallSites(rn, enclProc(exit))
      if followingNodes(c) contains rn
    } yield c -> r

  def getCallSites(node: Node, proc: Procedure): Iterator[Node] =
    supergraph getCallSites (node, proc)

  /**
   * All call nodes inside of a given procedure
   */
  def callNodesInProc(p: Procedure): Iterable[Node] = {
    val nodesInProc = DFS.getReachableNodes(
      supergraph,
      (supergraph getEntriesForProcedure p).toIterable,
      new Predicate[Node]() {
        override def test(n: Node): Boolean = enclProc(n) == p
      }
    )
    nodesInProc filter supergraph.isCall
  }

  def traverseSupergraph = supergraph.iterator
}
