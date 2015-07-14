package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.IdeProblem
import ca.uwaterloo.ide.util.Time.time

trait IdeSolver extends JumpFuncs with ComputeValues { this: IdeProblem with PropagatorI =>

  /**
   * Runs the IDE instance defined in IdeProblem.
   */
  lazy val solvedResult: Map[XNode, LatticeElem] = {
    // computeJumpFuncs corresponds to Phase I of the algorithm, computeValues corresponds to Phase II.
    timeComputeValues(time("computing jump functions") { computeJumpFuncs })
  }
}
