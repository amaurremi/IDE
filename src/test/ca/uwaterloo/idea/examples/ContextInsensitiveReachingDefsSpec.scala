package ca.uwaterloo.idea.examples

import ca.uwaterloo.ide.conversion.IdeFromIfdsBuilder
import com.ibm.wala.dataflow.IFDS.{TabulationProblem, ISupergraph}
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.{InstanceKey, PointerAnalysis}

class ContextInsensitiveReachingDefsSpec extends IdeFromIfdsBuilder {


  override type F = this.type
  override val walaIfdsProblem: TabulationProblem[ContextInsensitiveReachingDefsSpec.this.type, ContextInsensitiveReachingDefsSpec.this.type, F] = _
  override val Λ: Int = _
  override type Node = this.type
  override type Procedure = this.type
  override val callGraph: CallGraph = _
  override val entryPoints: Seq[Node] = _
  override val supergraph: ISupergraph[Node, Procedure] = _
  override val pointerAnalysis: PointerAnalysis[InstanceKey] = _

  override def phiInstructions(node: Node): Seq[ContextInsensitiveReachingDefsSpec.this.type] = ???

  override type PhiInstruction = this.type
}
