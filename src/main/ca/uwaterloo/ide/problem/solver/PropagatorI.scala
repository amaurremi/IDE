package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.PartiallyBalancedIdeFlowFunctions
import ca.uwaterloo.ide.problem.types.LabeledExplodedGraphTypes
import com.ibm.wala.util.collections.HashSetMultiMap

import scala.collection.mutable

trait PropagatorI extends LabeledExplodedGraphTypes {

  // [1-2]
  val jumpFn = mutable.Map[XEdge, MicroFunction]() withDefault { _ => λTop } // for some reason, withDefalutValue doesn't work

  val pathWorklist = new mutable.Queue[XEdge]

  /**
   * Maps (sq, c, d4) to (d3, jumpFn) if JumpFn(sq, d3 -> c, d4) != Top
   * [28]
   */
  val forwardExitD3s = new HashSetMultiMap[(Node, XNode), (Fact, MicroFunction)]
  
  def propagate(e: XEdge, f: MicroFunction)

  /**
   * returns:
   * jumpFn(e) != f ⊓ jumpFn(e)
   */
  def balancedPropagate(e: XEdge, f: MicroFunction): Boolean = {
    val jf = jumpFn(e)
    val f2 = f ⊓ jf
    if (f2 != jf) {
      jumpFn += e -> f2
      if (f2 != λTop) forwardExitFromPropagate(e, f2)
      pathWorklist enqueue e
      true
    } else false
  }

  /**
   * For line [28], we need to retrieve all d3 values that match the condition. When we encounter
   * them here, we store them in the forwardExitD3s map.
   */
  def forwardExitFromPropagate(e: XEdge, f2: MicroFunction) {
    forwardExitD3s.put((e.source.n, e.target), (e.source.d, f2))
  }
}

trait BalancedPropagator extends PropagatorI {

  def propagate(e: XEdge, f: MicroFunction) = balancedPropagate(e, f)
}

trait PartiallyBalancedPropagator extends PropagatorI with TraverseGraph { this: PartiallyBalancedIdeFlowFunctions =>

  private[this] def wasUsedAsUnbalancedSeed(e: XEdge): Boolean =
    unbalancedSeeds.contains(e.source)

  private[this] val unbalancedSeeds = mutable.Set[XNode](initialSeeds map {
    _._1.source
  }: _*)

  def propagate(e: XEdge, f: MicroFunction) {
    val n = e.target
    if (balancedPropagate(e, f) && wasUsedAsUnbalancedSeed(e) && n.isExitNode) {
      // e.target.d was reached from an entry seed. the facts that are reachable from that e.target.d
      // will be used as the new seeds
      for {
        r                 <- followingNodes(n.n)
        FactFunPair(d, f) <- unbalancedReturnFlowFunction(n, r)
        entry              = fakeEntry(r)
        s                  = XNode(entry, d)
      } {
        unbalancedSeeds add s
        propagate(XEdge(s, XNode(r, d)), f)
      }
    }
  }
}