package ca.uwaterloo.ide.conversion

import ca.uwaterloo.ide.problem.IdeProblem
import com.ibm.wala.dataflow.IFDS._
import com.ibm.wala.util.intset.IntIterator

import scala.collection.JavaConverters._
import scala.collection.breakOut

trait IdeFromIfdsBuilder extends IdeProblem {

  type F
  type Fact = Int

  val walaIfdsProblem: TabulationProblem[Node, Procedure, F]

  type IfdsEdgeFn      = (Node, Node) => Set[Fact]
  type IfdsOtherEdgeFn = XNode => Set[Fact]

  override type LatticeElem = IfdsLatticeElem
  override type IdeFunction = IfdsFunction

  override val Bottom: LatticeElem = IfdsBottom
  override val Top: LatticeElem    = IfdsTop
  override val Id: IdeFunction     = IfdsIdFunction
  override val λTop: IdeFunction   = IfdsTopFunction

  private[this] def walaFlowFunctionMap = walaIfdsProblem.getFunctionMap

  override def normalFlowFunction(src: XNode, dest: Node) =
    zipWithId(walaFlowFunctionMap.getNormalFlowFunction(src.n, dest).getTargets(src.d).intIterator)

  override def returnFlowFunction(call: Node, src: XNode, dest: Node): ReturnFlowFunctionType = {
    walaFlowFunctionMap.getReturnFlowFunction(call, src.n, dest) match {
      case u: IUnaryFlowFunction        =>
        UnaryReturnFlowFunction(zipWithId(u.getTargets(src.d).intIterator))
      case b: IBinaryReturnFlowFunction =>
        BinaryReturnFlowFunction(
          d4 =>
            zipWithId(b.getTargets(d4, src.d).intIterator))
    }
  }

  override def callToReturnFlowFunction(src: XNode, dest: Node)
    = zipWithId(walaFlowFunctionMap.getCallToReturnFlowFunction(src.n, dest).getTargets(src.d).intIterator)

  override def callFlowFunction(src: XNode, dest: Node, ret: Node)
    = zipWithId(walaFlowFunctionMap.getCallFlowFunction(src.n, dest, ret).getTargets(src.d).intIterator())

  override def callNoneToReturnFlowFunction(src: XNode, dest: Node)
    = zipWithId(walaFlowFunctionMap.getCallNoneToReturnFlowFunction(src.n, dest).getTargets(src.d).intIterator)

  private[this] def zipWithId(intIterator: IntIterator): Iterable[FactFunPair] =
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

  // todo check that this is the right assumption
  override val Λ: Int = 0

  override def entryPoints: Seq[(XEdge, IdeFunction)] = (walaIfdsProblem.initialSeeds().asScala map {
    e: PathEdge[Node] =>
      (XEdge(XNode(e.getEntry, e.getD1), XNode(e.getTarget, e.getD2)), Id)
  })(breakOut)

  override def supergraph: ISupergraph[Node, Procedure] = walaIfdsProblem.getSupergraph
}