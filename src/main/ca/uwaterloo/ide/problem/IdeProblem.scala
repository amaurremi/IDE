package ca.uwaterloo.ide.problem

import ca.uwaterloo.ide.types.{Phis, LabeledExplodedGraphTypes}
import ca.uwaterloo.ide.util.TraverseGraph

trait IdeProblem extends LabeledExplodedGraphTypes with IdeFlowFunctions with TraverseGraph with Phis
