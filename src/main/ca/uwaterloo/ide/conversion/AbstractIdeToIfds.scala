package ca.uwaterloo.ide.conversion

import ca.ide.problem.IdeProblem
import ca.ide.solver.IdeSolver
import ca.uwaterloo.ide.problem.IdeProblem
import ca.uwaterloo.ide.solver.IdeSolver

trait AbstractIdeToIfds extends IdeProblem with IfdsProblemWrapper with IdeSolver {

  def ifdsResult: Map[Node, Set[Fact]]
}
