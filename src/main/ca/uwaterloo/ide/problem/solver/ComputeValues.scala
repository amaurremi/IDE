package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.IdeProblem
import ca.uwaterloo.ide.util.Time.time

import scala.collection.{breakOut, mutable}

// p. 149 of Sagiv, Reps, Horwitz, "Precise inter-procedural dataflow instance
// with applications to constant propagation"
trait ComputeValues { this: IdeProblem with TraverseGraph =>

  private[this] type JumpFn = Map[XEdge, MicroFunction]
  private[this] type JumpFnConvenient = Map[(XNode, Node), Set[FactFunPair]]

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

  def timeComputeValues(jumpFn: JumpFn): Map[XNode, LatticeElem] =
    time("computing values") { computeValues(jumpFn) }

  def computeValues(jumpFn: JumpFn): Map[XNode, LatticeElem]  = {
    // Phase II(i)
    initialize()
    // [4-17]
    val jumpFnConvenient = toConvenient(jumpFn)
    while (nodeWorklist.nonEmpty) {
      val node = nodeWorklist.dequeue()
      if (node.isStartNode)
        computeStartNode(node, jumpFnConvenient)
      if (node.isCallNode)
        computeCallNode(node)
    }
    // Phase II(ii)
    for {
      (XEdge(sp, n), fPrime) <- jumpFn
      if fPrime != λTop
      if !(n.isCallNode || n.isStartNode)
    } {
      vals += n -> vals(n) ⊓ fPrime(vals(sp))
    }
    vals.toMap
  }

  private[this] def toConvenient(jumpFn: JumpFn): JumpFnConvenient =
    jumpFn.foldLeft(Map.empty[(XNode, Node), Set[FactFunPair]]) {
      case (prevMap, (XEdge(srcNode, XNode(targetN, targetD)), fun)) =>
        val key = (srcNode, targetN)
        val oldVal: Set[FactFunPair] = prevMap getOrElse(key, Set.empty[FactFunPair])
        prevMap updated(key, oldVal + FactFunPair(targetD, fun))
    }

  private[this] def computeCallNode(c: XNode) {
    val cn = c.n
    for {
      sq                          <- targetStartNodes(cn)
      r                           <- returnNodes(cn, Some(sq))
      FactFunPair(dPrime, edgeFn) <- callFlowFunction(c, sq, r)
    } {
      propagateValue(XNode(sq, dPrime), edgeFn(vals(c)))
    }
  }

  private[this] def getJumpFnTargetFacts(ideNode1: XNode, node2: Node, jumpFnConvenient: JumpFnConvenient): Set[FactFunPair] =
    jumpFnConvenient getOrElse ((ideNode1, node2), Set.empty[FactFunPair])

  // [8-10]
  private[this] def computeStartNode(sp: XNode, jumpFuncConvenient: JumpFnConvenient) {
    for {
      c                           <- callNodesInProc(enclProc(sp.n))
      FactFunPair(dPrime, fPrime) <- getJumpFnTargetFacts(sp, c, jumpFuncConvenient)
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
