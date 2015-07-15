package ca.uwaterloo.ide.problem.types

trait LabeledExplodedGraphTypes extends ExplodedGraphTypes {

  /**
   * The type for micro functions that correspond to the edges in the exploded supergraph
   */
  type MicroFunction <: MicroFunctionI

  /**
   * Represents λl.⊤
   */
  val λTop: MicroFunction

  /**
   * Represents λl.l
   */
  val Id: MicroFunction

  /**
   * The type for a lattice element for the set L
   */
  type LatticeElem <: LatticeElementI[LatticeElem]

  /**
   * Lattice top element
   */
  val Top: LatticeElem

  /**
   * Lattice bottom element
   */
  val Bottom: LatticeElem

  /**
   * A lattice for elements of the set L
   */
  trait LatticeElementI [L <: LatticeElementI[L]] {

    def ⊓(el: L): L

    def ⊔(el: L): L = throw new UnsupportedOperationException("Join operation not defined.")

    override def equals(o: Any): Boolean
  }

  /**
   * An IDE function that corresponds to an edge in the exploded supergraph
   */
  trait MicroFunctionI {

    def apply(arg: LatticeElem): LatticeElem

    /**
     * Meet operator
     */
    def ⊓(f: MicroFunction): MicroFunction

    /**
     * Compose operator
     */
    def ◦(f: MicroFunction): MicroFunction
  }

  case class Seed(edge: XEdge, f: MicroFunction)

  /**
   * The main method nodes that should be the entry points for the instance
   */
  def initialSeeds: Seq[(XEdge, MicroFunction)]
}
