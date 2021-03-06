package ca.uwaterloo.ide.conversion

import ca.uwaterloo.ide.problem.IdeProblem
import com.ibm.wala.dataflow.IFDS._
import com.ibm.wala.util.intset.{IntIterator, IntSet, SparseIntSet}

import scala.collection.JavaConverters._
import scala.collection.breakOut

trait IdeFromIfdsBuilder extends IdeProblem {

  type F
  type Fact = Int

  def walaIfdsProblem: TabulationProblem[Node, Procedure, F]

  type IfdsEdgeFn      = (Node, Node) => Set[Fact]
  type IfdsOtherEdgeFn = XNode => Set[Fact]

  override type LatticeElem = IfdsLatticeElem
  override type MicroFunction = IfdsFunction

  override val Bottom: LatticeElem = IfdsBottom
  override val Top: LatticeElem    = IfdsTop
  override val Id: MicroFunction     = IfdsIdFunction
  override val λTop: MicroFunction   = IfdsTopFunction

  private[this] def walaFlowFunctionMap = walaIfdsProblem.getFunctionMap

  override def normalFlowFunction(src: XNode, dest: Node) =
    zipWithId(unaryIterator(walaFlowFunctionMap.getNormalFlowFunction(src.n, dest), src))

  override def returnFlowFunction(call: Node, src: XNode, dest: Node): ReturnFlowFunctionType = {
    walaFlowFunctionMap.getReturnFlowFunction(call, src.n, dest) match {
      case u: IUnaryFlowFunction        =>
        UnaryReturnFlowFunction(zipWithId(unaryIterator(u, src)))
      case b: IBinaryReturnFlowFunction =>
        BinaryReturnFlowFunction(
          d4 =>
            zipWithId(binaryIterator(b, src, d4)))
    }
  }

  override def callToReturnFlowFunction(src: XNode, dest: Node)
    = zipWithId(unaryIterator(walaFlowFunctionMap.getCallToReturnFlowFunction(src.n, dest), src))

  override def callFlowFunction(src: XNode, dest: Node, ret: Node)
    = zipWithId(unaryIterator(walaFlowFunctionMap.getCallFlowFunction(src.n, dest, ret), src))

  override def callNoneToReturnFlowFunction(src: XNode, dest: Node)
    = zipWithId(unaryIterator(walaFlowFunctionMap.getCallNoneToReturnFlowFunction(src.n, dest), src))

  def unaryIterator(flowFunction: IUnaryFlowFunction, src: XNode): Iterable[Fact] =
    if (flowFunction == null) Seq()
    else {
      val targets: IntSet = flowFunction.getTargets(src.d)
      if (targets == null) Seq()
      else intIteratorToScala(targets.intIterator)
    }

  private[this] def binaryIterator(flowFunction: IBinaryReturnFlowFunction, src: XNode, d: Fact): Iterable[Fact] =
    if (flowFunction == null) Seq()
    else {
      val targets: SparseIntSet = flowFunction.getTargets(d, src.d)
      if (targets == null) Seq()
      else intIteratorToScala(targets.intIterator)
    }

  def zipWithId(iterator: Iterable[Fact]): Iterable[FactFunPair] =
    iterator map { FactFunPair(_, IfdsIdFunction) }

  trait IfdsLatticeElem extends LatticeElementI[IfdsLatticeElem]

  case object IfdsTop extends IfdsLatticeElem {
    override def ⊓(el: IfdsLatticeElem): IfdsLatticeElem = el
  }

  case object IfdsBottom extends IfdsLatticeElem {
    override def ⊓(el: IfdsLatticeElem): IfdsLatticeElem = IfdsBottom
  }

  trait IfdsFunction extends MicroFunctionI

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

  private[this] def intIteratorToScala(intIterator: IntIterator): Iterable[Int] =
    new Iterator[Int] {
      override def hasNext: Boolean = intIterator.hasNext

      override def next(): Int = intIterator.next()
    }.toIterable

  // todo check that this is the right assumption
  override val Λ: Int = 0

  override def initialSeeds: Seq[(XEdge, MicroFunction)] = (walaIfdsProblem.initialSeeds().asScala map {
    e: PathEdge[Node] => 
      (XEdge(XNode(e.getEntry, e.getD1), XNode(e.getTarget, e.getD2)), Id)
  })(breakOut)

  override def supergraph: ISupergraph[Node, Procedure] = walaIfdsProblem.getSupergraph
}