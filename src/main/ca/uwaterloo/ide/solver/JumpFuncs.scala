package ca.uwaterloo.ide.solver

import com.ibm.wala.util.collections.HashSetMultiMap
import ca.ide.problem.IdeProblem
import ca.ide.util.TraverseGraph
import ca.uwaterloo.ide.problem.IdeProblem
import ca.uwaterloo.ide.util.TraverseGraph

import scala.collection.JavaConverters._
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
    // [5]
    val edges = entryPoints map {
      ep =>
        val zeroNode = XNode(ep, Λ)
        XEdge(zeroNode, zeroNode)
    }
    pathWorklist enqueue (edges: _*)
    // [6]
    jumpFn ++= (edges map {
      _ -> Id
    })(breakOut)
  }

  /**
   * Maps (sq, c, d4) to (d3, jumpFn) if JumpFn(sq, d3 -> c, d4) != Top
   * [28]
   */
  private[this] val forwardExitD3s = new HashSetMultiMap[(NodeType, XNode), (Fact, IdeFunction)]

  def computeJumpFuncs: Map[XEdge, IdeFunction] = {
    initialize()
    // [7-33]
    while (pathWorklist.size > 0) {
      // [8-9] e = (sp, d1) -> (n, d2)
      val e = pathWorklist.dequeue()
      val f = jumpFn(e)
      val n = e.target
      n.n match {
        case PhiNode(pn) =>
          forwardAnyNode(e, f)
        case NormalNode(nn) =>
          if (n.isCallNode)
            forwardCallNode(e, f)
          if (n.isExitNode)
            forwardExitNode(e, f)
          if (!n.isCallNode && !n.isExitNode)
            forwardAnyNode(e, f)
      }
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
    for {
      sq <- targetStartNodes(node)
      d3 <- callStartD2s(n, sq)
    } {
      val sqn = XNode(sq, d3)
      propagate(XEdge(sqn, sqn), Id)
      forwardExitFromCall(n, f, sqn)
    }
    // [14-16]
    for {
      r                       <- returnNodes(node)
      FactFunPair(d3, edgeFn) <- callToReturnFlowFunction(n, r)
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

  def forwardExitNodeSpecific(e: XEdge, f: IdeFunction, call: XNode, r: NodeType) = {
    val sp = e.source
    val c = call.n
    val d4 = call.d
    for {
      FactFunPair(d1, f4) <- callFlowFunction(XNode(c, d4), sp.n)
      if sp.d == d1
      FactFunPair(d5, f5) <- returnFlowFunction(e.target, r)
      rn = XNode(r, d5)
      sumEdge = XEdge(XNode(c, d4), rn)
      sumF = summaryFn(sumEdge)
      fPrime = (f5 ◦ f ◦ f4) ⊓ sumF
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
    c: NodeType,
    d4: Fact,
    rn: XNode,
    fPrime: IdeFunction
  ) {
    for {
      sq <- startNodes(c.node)
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
      r        <- returnNodes(call.n)
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
      m <- followingNodes(n.n).toSeq
      FactFunPair(d3, edgeFn) <- otherSuccEdgesWithPhi(n, m)
    } {
      propagate(XEdge(e.source, XNode(m, d3)), edgeFn ◦ f)
    }
  }

  private[this] def otherSuccEdgesWithPhi(node: XNode, dest: NodeType) =
    node.n match {
      case NormalNode(n) =>
        normalFlowFunction(node, dest)
      case PhiNode(n) =>
        normalPhiFlowFunction(node, dest)
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

    private[this] val forwardExitD4s = new HashSetMultiMap[(NodeType, XNode), Fact]
    private[this] val queriedExit = new HashSetMultiMap[(NodeType, NodeType), (XEdge, IdeFunction)]

    def put(call: NodeType, sp: XNode, d: Fact) {
      forwardExitD4s.put((call, sp), d)
    }

    def get(e: XEdge, f: IdeFunction)(call: NodeType, sp: XNode): Set[Fact] = {
      queriedExit.put((call, sp.n), (e, f))
      forwardExitD4s.get(call, sp).asScala.toSet
    }

    def getQueried(call: NodeType, sp: NodeType): Set[(XEdge, IdeFunction)] =
      queriedExit.get((call, sp)).asScala.toSet
  }
}
