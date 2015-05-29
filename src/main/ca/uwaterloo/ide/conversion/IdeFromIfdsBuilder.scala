package ca.uwaterloo.ide.conversion

import ca.uwaterloo.ide.problem.IdeProblem
import com.ibm.wala.dataflow.IFDS.{IUnaryFlowFunction, IBinaryReturnFlowFunction, TabulationProblem}
import com.ibm.wala.util.intset.{IntSet, IntIterator}

trait IdeFromIfdsBuilder extends IdeProblem {

//  T = Node, P = Procedure
  type Fact = Int
  type F
  val walaIfdsProblem: TabulationProblem[Node, Procedure, F]

  type IfdsEdgeFn      = (Node, Node) => Set[Fact]
  type IfdsOtherEdgeFn = XNode => Set[Fact]

  override type LatticeElem = IfdsLatticeElem
  override type IdeFunction = IfdsFunction

  override val Bottom: LatticeElem = IfdsBottom
  override val Top: LatticeElem    = IfdsTop
  override val Id: IdeFunction     = IfdsIdFunction
  override val λTop: IdeFunction   = IfdsTopFunction

  private[this] val walaFlowFunctionMap = walaIfdsProblem.getFunctionMap

  override def normalFlowFunction(node: XNode, tpe: NodeType) =
    zipWithId(walaFlowFunctionMap.getNormalFlowFunction(node.n.node, tpe.node).getTargets(node.d).intIterator)

  override def normalPhiFlowFunction(node: XNode, tpe: NodeType)
    = zipWithId(???)

  override def returnFlowFunction(call: NodeType, node: XNode, tpe: NodeType) = ???

  override def callToReturnFlowFunction(node: XNode, tpe: NodeType)
    = zipWithId(walaFlowFunctionMap.getCallToReturnFlowFunction(node.n, tpe.node).getTargets(node.d).intIterator)(node, tpe)

  override def callFlowFunction(node: XNode, tpe: NodeType)
    = zipWithId(walaFlowFunctionMap.getCallFlowFunction())(node, tpe)
  override def callNoneToReturnFlowFunction(node: XNode, tpe: NodeType) = ???

  private[this] def zipWithId(intIterator: IntIterator) =
    intIteratorToScala(intIterator) map { FactFunPair(_, IfdsIdFunction) }

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

  private[this] def intIteratorToScala(intIterator: IntIterator): Iterable[Int] = {
    var set = Iterable[Int]()
    while (intIterator.hasNext) {
      set ++= Seq(intIterator.next)
    }
    set
  }
}
