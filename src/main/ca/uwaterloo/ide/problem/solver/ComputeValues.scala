package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.IdeProblem

import scala.collection.{breakOut, mutable}

// p. 149 of Sagiv, Reps, Horwitz, "Precise inter-procedural dataflow instance
// with applications to constant propagation"
trait ComputeValues { this: IdeProblem with TraverseGraph =>

  private[this] type JumpFn = Map[XEdge, IdeFunction]

  // [1]
  private[this] val vals = mutable.Map[XNode, LatticeElem]() withDefault { _ => Top }

  private[this] lazy val nodeWorklist = mutable.Queue[XNode]()

  private[this] def initialize() {
    // [2]
    vals ++= (initialSeeds map {
      case (XEdge(n, _), _) =>
        n -> Bottom
    })(breakOut)
    // [3]
    nodeWorklist ++= initialSeeds map {
      case (XEdge(n, _), _) => n
    }
  }

  def computeValues(jumpFunc: JumpFn): Map[XNode, LatticeElem]  = {
    // Phase II(i)
    initialize()
    // [4-17]
    while (nodeWorklist.nonEmpty) {
      val node = nodeWorklist.dequeue()
      if (node.isStartNode)
        computeStartNode(node, jumpFunc)
      if (node.isCallNode)
        computeCallNode(node)
    }
    // Phase II(ii)
    for {
      (XEdge(sp, n), fPrime) <- jumpFunc
      if fPrime != λTop
      if !(n.isCallNode || n.isStartNode)
    } {
      vals += n -> vals(n) ⊓ fPrime(vals(sp))
    }
    vals.toMap
  }

  private[this] def computeCallNode(c: XNode) {
    val cn = c.n
    for {
      sq                          <- targetStartNodes(cn)
      r                           <- returnNodes(cn, sq)
      FactFunPair(dPrime, edgeFn) <- callFlowFunction(c, sq, r)
    } {
      propagateValue(XNode(sq, dPrime), edgeFn(vals(c)))
    }
  }

  private[this] def getJumpFnTargetFacts(ideNode1: XNode, node2: Node, jumpFn: JumpFn): Set[FactFunPair] =
    (jumpFn collect {
      case (XEdge(source, XNode(n, d)), f)
        if source == ideNode1 && n == node2 =>
          FactFunPair(d, f)
    })(breakOut)

  // [8-10]
  private[this] def computeStartNode(sp: XNode, jumpFunc: JumpFn) {
    for {
      c                           <- callNodesInProc(enclProc(sp.n))
      FactFunPair(dPrime, fPrime) <- getJumpFnTargetFacts(sp, c, jumpFunc)
      if fPrime != λTop
    } {
      propagateValue(XNode(c, dPrime), fPrime(vals(sp)))
    }
  }

  private[this] def propagateValue(n: XNode, v: LatticeElem) {
    val ln = vals(n)
    val v2 = v ⊓ ln
    if (v2 != ln) {
      vals += n -> v2
      nodeWorklist enqueue n
    }
  }
}
