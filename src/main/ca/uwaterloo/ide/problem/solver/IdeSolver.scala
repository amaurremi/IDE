package ca.uwaterloo.ide.problem.solver

import ca.uwaterloo.ide.problem.IdeProblem

trait IdeSolver extends JumpFuncs with ComputeValues { this: IdeProblem =>

  /**
   * Runs the IDE instance defined in IdeProblem.
   */
  lazy val solvedResult: Map[XNode, LatticeElem] = {
    // computeJumpFuncs corresponds to Phase I of the algorithm, computeValues corresponds to Phase II.
    computeValues(computeJumpFuncs)
  }
}
