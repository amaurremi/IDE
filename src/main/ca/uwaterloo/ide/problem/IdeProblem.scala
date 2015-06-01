package ca.uwaterloo.ide.problem

import ca.uwaterloo.ide.solver.TraverseGraph
import ca.uwaterloo.ide.types.LabeledExplodedGraphTypes

trait IdeProblem extends LabeledExplodedGraphTypes with IdeFlowFunctions with TraverseGraph
