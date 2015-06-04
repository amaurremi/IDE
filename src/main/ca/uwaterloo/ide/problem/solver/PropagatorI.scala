package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.types.LabeledExplodedGraphTypes
import com.ibm.wala.util.collections.HashSetMultiMap

import scala.collection.mutable

trait PropagatorI extends LabeledExplodedGraphTypes {

  // [1-2]
  val jumpFn = mutable.Map[XEdge, IdeFunction]() withDefault { _ => λTop } // for some reason, withDefalutValue doesn't work

  val pathWorklist = new mutable.Queue[XEdge]

  /**
   * Maps (sq, c, d4) to (d3, jumpFn) if JumpFn(sq, d3 -> c, d4) != Top
   * [28]
   */
  val forwardExitD3s = new HashSetMultiMap[(Node, XNode), (Fact, IdeFunction)]
  
  def propagate(e: XEdge, f: IdeFunction)

  /**
   * For line [28], we need to retrieve all d3 values that match the condition. When we encounter
   * them here, we store them in the forwardExitD3s map.
   */
  def forwardExitFromPropagate(e: XEdge, f2: IdeFunction) {
    forwardExitD3s.put((e.source.n, e.target), (e.source.d, f2))
  }
}

trait BalancedPropagator extends PropagatorI {

  def propagate(e: XEdge, f: IdeFunction) {
    val jf = jumpFn(e)
    val f2 = f ⊓ jf
    if (f2 != jf) {
      jumpFn += e -> f2
      if (f2 != λTop) forwardExitFromPropagate(e, f2)
      pathWorklist enqueue e
    }
  }
}

trait PartiallyBalancedPropagator extends PropagatorI {

  // todo make unblanced
  def propagate(e: XEdge, f: IdeFunction) {
    val jf = jumpFn(e)
    val f2 = f ⊓ jf
    if (f2 != jf) {
      jumpFn += e -> f2
      if (f2 != λTop) forwardExitFromPropagate(e, f2)
      pathWorklist enqueue e
    }
  }
}