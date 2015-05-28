package ca.uwaterloo.ide.types

trait Phis { this: SuperGraphTypes =>

  type PhiInstruction

  def phiInstructions(node: Node): Seq[PhiInstruction]
}
