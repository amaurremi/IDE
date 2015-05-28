package ca.uwaterloo.ide.solver

import ca.ide.problem.IdeProblem
import ca.ide.types.Phis
import ca.ide.util.TraverseGraph
import ca.uwaterloo.ide.problem.IdeProblem
import ca.uwaterloo.ide.types.Phis
import ca.uwaterloo.ide.util.TraverseGraph

trait IdeSolver extends JumpFuncs with ComputeValues with TraverseGraph with Phis { this: IdeProblem =>

  /**
   * Runs the IDE instance defined in IdeProblem.
   */
  lazy val solvedResult: Map[XNode, LatticeElem] = {
    // computeJumpFuncs corresponds to Phase I of the algorithm, computeValues corresponds to Phase II.
    computeValues(computeJumpFuncs)
  }
}
