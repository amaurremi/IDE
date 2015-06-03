package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.IdeProblem
import com.ibm.wala.util.collections.HashSetMultiMap

import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.collection.{breakOut, mutable}

// p. 147 of Sagiv, Reps, Horwitz, "Precise interprocedural dataflow instance
// with applications to constant propagation"
trait JumpFuncs {
  this: IdeProblem with TraverseGraph =>

  private[this] val pathWorklist = new mutable.Queue[XEdge]

  // [1-2]
  private[this] val jumpFn = mutable.Map[XEdge, IdeFunction]() withDefault { _ => λTop} // for some reason, withDefalutValue doesn't work

  // [3-4]
  private[this] val summaryFn = mutable.Map[XEdge, IdeFunction]() withDefault { _ => λTop}

  private[this] val forwardExitD4s = new ForwardExitD4s

  def initialize() {
    pathWorklist enqueue (initialSeeds.unzip._1: _*)
    // [6]
    jumpFn ++= initialSeeds
  }

  /**
   * Maps (sq, c, d4) to (d3, jumpFn) if JumpFn(sq, d3 -> c, d4) != Top
   * [28]
   */
  private[this] val forwardExitD3s = new HashSetMultiMap[(Node, XNode), (Fact, IdeFunction)]

  def computeJumpFuncs: Map[XEdge, IdeFunction] = {
    initialize()
    // [7-33]
    while (pathWorklist.nonEmpty) {
      // [8-9] e = (sp, d1) -> (n, d2)
      val e = pathWorklist.dequeue()
      val f = jumpFn(e)
      val n = e.target
      if (n.isCallNode)
        forwardCallNode(e, f)
      if (n.isExitNode)
        forwardExitNode(e, f)
      if (!n.isCallNode && !n.isExitNode)
        forwardAnyNode(e, f)
    }
    jumpFn.toMap
  }

  /**
   * p. 147, [11-18]
   */
  private[this] def forwardCallNode(e: XEdge, f: IdeFunction) {
    val n = e.target
    // [12-13]
    val node = n.n
    val targetNodes = targetStartNodes(node)
    for {
      sq <- targetNodes
      r  <- returnNodes(node, Some(sq))
      d3 <- callStartD2s(n, sq, r)
    } {
      val sqn = XNode(sq, d3)
      propagate(XEdge(sqn, sqn), Id)
      forwardExitFromCall(n, f, sqn)
    }
    // [14-16]
    if (targetNodes.isEmpty)
      forwardCallReturn(e, None, f)
    else
      targetNodes foreach {
        sq =>
          forwardCallReturn(e, Some(sq), f)
      }
  }

  private[this] def forwardCallReturn(e: XEdge, sq: Option[Node], f: IdeFunction) = {
    val n = e.target
    for {
      r                       <- returnNodes(n.n, sq) // todo I think just having None is incorrect
      FactFunPair(d3, edgeFn) <- callNoneToReturnFlowFunction(n, r)
      rn                       = XNode(r, d3)
      re                       = XEdge(e.source, rn)
    } {
      propagate(re, edgeFn ◦ f)
      // [17-18]
      val f3 = summaryFn(XEdge(n, rn))
      if (f3 != λTop)
        propagate(re, f3 ◦ f)
    }
  }

  def forwardExitNodeSpecific(e: XEdge, f: IdeFunction, call: XNode, r: Node) = {
    val sp           = e.source
    val XNode(c, d4) = call
    val returnPairs: Iterable[FactFunPair] =
      returnFlowFunction(c, e.target, r) match {
        case BinaryReturnFlowFunction(fun)   =>
          fun(d4)
        case UnaryReturnFlowFunction(ffps) =>
          ffps
      }
    for {
      FactFunPair(d1, f4) <- callFlowFunction(call, sp.n, r)
      if sp.d == d1
      FactFunPair(d5, f5) <- returnPairs
      rn                   = XNode(r, d5)
      sumEdge              = XEdge(call, rn)
      sumF                 = summaryFn(sumEdge)
      fPrime               = (f5 ◦ f ◦ f4) ⊓ sumF
      if fPrime != sumF
    } {
      // [26]
      summaryFn += sumEdge -> fPrime
      // [29]
      forwardExitPropagate(c, d4, rn, fPrime)
    }
  }

  private[this] def forwardExitNode(e: XEdge, f: IdeFunction) {
    for {
      (c, r) <- callReturnPairs(e.target.n)
      d4     <- forwardExitD4s.get(e, f)(c, e.source)
    } {
      forwardExitNodeSpecific(e, f, XNode(c, d4), r)
    }
  }

  private[this] def forwardExitPropagate(
    c: Node,
    d4: Fact,
    rn: XNode,
    fPrime: IdeFunction
  ) {
    for {
      sq <- startNodes(c)
      (d3, f3) <- forwardExitD3s.get(sq, XNode(c, d4)).asScala
      if f3 != λTop
    } {
      // [29]
      propagate(XEdge(XNode(sq, d3), rn), fPrime ◦ f3)
    }
  }

  /**
   * To get d4 values in line [21], we need to remember all tuples (c, d4, sp) when we encounter them
   * in the call-processing procedure.
   */
  private[this] def forwardExitFromCall(call: XNode, f: IdeFunction, sq: XNode) {
    forwardExitD4s.put(call.n, sq, call.d)
    for {
      (e2, f2) <- forwardExitD4s.getQueried(call.n, sq.n)
      r        <- returnNodes(call.n, Some(sq.n))
    } {
      forwardExitNodeSpecific(e2, f2, call, r)
    }
  }

  /**
   * For line [28], we need to retrieve all d3 values that match the condition. When we encounter
   * them here, we store them in the forwardExitD3s map.
   */
  private[this] def forwardExitFromPropagate(e: XEdge, f2: IdeFunction) {
    forwardExitD3s.put((e.source.n, e.target), (e.source.d, f2))
  }

  private[this] def forwardAnyNode(e: XEdge, f: IdeFunction) {
    val n = e.target
    for {
      m <- followingNodes(n.n)
      FactFunPair(d3, edgeFn) <- normalFlowFunction(n, m)
    } {
      propagate(XEdge(e.source, XNode(m, d3)), edgeFn ◦ f)
    }
  }

  private[this] def propagate(e: XEdge, f: IdeFunction) {
    val jf = jumpFn(e)
    val f2 = f ⊓ jf
    if (f2 != jf) {
      jumpFn += e -> f2
      if (f2 != λTop) forwardExitFromPropagate(e, f2)
      pathWorklist enqueue e
    }
  }

  /**
   * Maps (c, sp, d1) to EdgeFn(d4, f)
   * [21]
   */
  private class ForwardExitD4s {

    private[this] val forwardExitD4s = new HashSetMultiMap[(Node, XNode), Fact]
    private[this] val queriedExit = new HashSetMultiMap[(Node, Node), (XEdge, IdeFunction)]

    def put(call: Node, sp: XNode, d: Fact) {
      forwardExitD4s.put((call, sp), d)
    }

    def get(e: XEdge, f: IdeFunction)(call: Node, sp: XNode): Set[Fact] = {
      queriedExit.put((call, sp.n), (e, f))
      forwardExitD4s.get(call, sp).asScala.toSet
    }

    def getQueried(call: Node, sp: Node): Set[(XEdge, IdeFunction)] =
      queriedExit.get((call, sp)).asScala.toSet
  }
}
