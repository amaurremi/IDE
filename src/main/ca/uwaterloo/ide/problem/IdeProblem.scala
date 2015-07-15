package ca.uwaterloo.ide.problem

import ca.uwaterloo.ide.problem.solver.{PropagatorI, IdeSolver, TraverseGraph}
import ca.uwaterloo.ide.problem.types.LabeledExplodedGraphTypes

trait IdeProblem extends LabeledExplodedGraphTypes with IdeFlowFunctions with TraverseGraph
