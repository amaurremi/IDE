package ca.uwaterloo.ide.conversion

import ca.uwaterloo.ide.problem.IdeProblem
import com.ibm.wala.dataflow.IFDS.TabulationProblem

trait IdeFromIfdsBuilder extends IdeProblem {

  type T; type P; type F // todo get rid of those?
  val walaIfdsProblem: TabulationProblem[T, P, F]

  override type LatticeElem = IfdsLatticeElem
  override type IdeFunction = IfdsFunction

  override val Bottom: LatticeElem = IfdsBottom
  override val Top: LatticeElem    = IfdsTop
  override val Id: IdeFunction     = IfdsIdFunction
  override val λTop: IdeFunction   = IfdsTopFunction

  override def otherSuccEdges(node: XNode)                   = zipWithIdOther(ifdsOtherSuccEdges)(node)
  override def otherSuccEdgesPhi(node: XNode)                = zipWithIdOther(ifdsOtherSuccEdgesPhi)(node)
  override def endReturnEdges(node: XNode, tpe: NodeType)    = zipWithId(ifdsEndReturnEdges)(node, tpe)
  override def callReturnEdges(node: XNode, tpe: NodeType)   = zipWithId(ifdsCallReturnEdges)(node, tpe)
  override def callStartEdges(node: XNode, tpe: NodeType)    = zipWithId(ifdsCallStartEdges)(node, tpe)

  private[this] def zipWithId(f: IfdsEdgeFn)(ideN1: XNode, d1: NodeType) =
    f(ideN1, d1) map { FactFunPair(_, IfdsIdFunction) }

  private[this] def zipWithIdOther(f: IfdsOtherEdgeFn)(ideN1: XNode) =
    f(ideN1) map { FactFunPair(_, IfdsIdFunction) }

  trait IfdsLatticeElem extends Lattice[IfdsLatticeElem]

  case object IfdsTop extends IfdsLatticeElem {
    override def ⊓(el: IfdsLatticeElem): IfdsLatticeElem = el
  }

  case object IfdsBottom extends IfdsLatticeElem {
    override def ⊓(el: IfdsLatticeElem): IfdsLatticeElem = IfdsBottom
  }

  trait IfdsFunction extends IdeFunctionI

  case object IfdsIdFunction extends IfdsFunction {

    override def apply(el: IfdsLatticeElem): IfdsLatticeElem = el

    override def ◦(f: IfdsFunction): IfdsFunction = f

    override def ⊓(f: IfdsFunction): IfdsFunction = IfdsIdFunction
  }

  case object IfdsTopFunction extends IfdsFunction {

    override def apply(el: IfdsLatticeElem): IfdsLatticeElem = IfdsTop

    override def ◦(f: IfdsFunction): IfdsFunction = IfdsTopFunction

    override def ⊓(f: IfdsFunction): IfdsFunction = f
  }
}
