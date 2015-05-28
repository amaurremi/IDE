package ca.uwaterloo.ide.util

import com.ibm.wala.classLoader.{IField, IMethod}
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.types.FieldReference
import ca.ide.types.ExplodedGraphTypes
import ca.uwaterloo.ide.types.ExplodedGraphTypes

trait VariableFacts extends ExplodedGraphTypes with TraverseGraph with WalaInstructions {

  type FactElem
  type ValueNumber = Int

  override type Fact   = VariableFact
  override val Λ: Fact = Lambda

  /**
   * Represents a fact for the set D
   */
  abstract sealed class VariableFact

  /**
   * @param elem The element that corresponds to the left-hand-side variable in an assignment
   */
  case class Variable(method: IMethod, elem: FactElem) extends VariableFact {
    override def toString: String = elem.toString + " in " + method.getName.toString + "()"
  }

  case object ReturnSecretValue extends VariableFact

  case object ArrayElement extends VariableFact

  case class Field(field: IField) extends VariableFact

  def getIField(cha: IClassHierarchy, f: FieldReference): IField =
    cha.resolveField(f)

  /**
   * Represents the Λ fact
   */
  case object Lambda extends VariableFact {
    override def toString: String = "Λ"
  }

  /**
   * Get value number from fact element
   */
  def getValNum(factElem: FactElem, node: XNode): ValueNumber
}
