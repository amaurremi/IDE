package ca.uwaterloo.ide.util

import ca.uwaterloo.ide.types.Phis
import com.ibm.wala.classLoader.IClass
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.impl.ClassHierarchyMethodTargetSelector
import com.ibm.wala.ipa.cfg.BasicBlockInContext
import com.ibm.wala.ssa._
import com.ibm.wala.ssa.analysis.IExplodedBasicBlock
import com.ibm.wala.types.MethodReference
import ca.ide.types.Phis
import ca.uwaterloo.ide.types.{ExplodedGraphTypes, Phis}

import scala.collection.JavaConverters._
import scala.collection.breakOut

trait WalaInstructions extends Phis { this: VariableFacts with ExplodedGraphTypes =>

  override type Node           = BasicBlockInContext[IExplodedBasicBlock]
  override type Procedure      = CGNode
  override type PhiInstruction = SSAPhiInstruction

  override def phiInstructions(node: Node) = node.iteratePhis().asScala.toSeq

  def firstParameter(instr: SSAInvokeInstruction): Int =
    if (instr.isStatic) 0 else 1

  def firstParameter(node: Node): Int =
    if (node.getMethod.isStatic) 0 else 1

  /**
   * If the variable corresponding to this node's fact is passed as a parameter to this call instruction,
   * returns the number of the parameter.
   */
  def getParameterNumber(node: XNode, callInstr: SSAInvokeInstruction): Option[Int] =
    node.d match {
      case Variable(method, elem)                               =>
        val valNum = getValNum(elem, node)
        firstParameter(callInstr) to callInstr.getNumberOfParameters - 1 find {
          callInstr.getUse(_) == valNum
        }
      case ArrayElement | Field(_) | Lambda | ReturnSecretValue => None // todo fields???
    }

  /**
   * If the variable corresponding to this node's fact is passed as a parameter to the enclosing method,
   * returns the number of the parameter.
   */
  def getParameterNumber(node: XNode): Option[Int] =
    node.d match {
      case Variable(method, elem)                               =>
        val valNum = getValNum(elem, node)
        val ir: IR = enclProc(node.n.node).getIR
        firstParameter(node.n.node) to node.n.node.getMethod.getNumberOfParameters - 1 find {
           ir.getParameter(_) == valNum
        }
      case ArrayElement | Field(_) | Lambda | ReturnSecretValue => None // todo fields???
    }

  /**
   * Get all instructions following instruction in node `n` in its procedure.
   */
  def followingInstructions(n: NodeType): Seq[SSAInstruction] =
    followingNodes(n) map { _.node.getLastInstruction }

  /**
   * Get the value number for the ith parameter.
   * @param argNum the number of the parameter
   * @param n a node inside of the method
   */
  def getValNumFromParameterNum(n: Node, argNum: Int): ValueNumber =
    enclProc(n).getIR.getSymbolTable.getParameter(argNum)

  def getCallNodes(exit: NodeType, ret: NodeType): Seq[Node] = {
    callReturnPairs(exit) collect {
      case (c: NodeType, r: NodeType) if r == ret => c.node
    }
  }

  def getCallInstructions(exit: NodeType, ret: NodeType): Seq[SSAInvokeInstruction] =
    getCallNodes(exit, ret) map {
      _.getLastInstruction match {
        case callInstr: SSAInvokeInstruction =>
          callInstr
        }
    }

  /**
   * Get the value number for the ith parameter.
   * @param argNum the number of the parameter, excluding this // todo account for non-static methods
   * @param instr the call instruction
   */
  def getValNumFromParameterNum(instr: SSAInvokeInstruction, argNum: Int): ValueNumber =
    instr.getUse(argNum)

  /**
   * The value number of a call's return value.
   */
  def callValNum(callInstr: SSAInvokeInstruction): Option[ValueNumber] =
    if (callInstr.getNumberOfReturnValues == 1)
      Some(callInstr.getReturnValue(0))
    else None
  
  /**
   * Value number of a constructor
   */
  def initValNum(method: MethodReference, callInstr: SSAInvokeInstruction): ValueNumber = {
    assert(method.isInit)
    callInstr.getUse(0)
  }

  def hasRetValue(retInstr: SSAReturnInstruction) = retInstr.getResult >= 0

  def getMethodName(node: Node): String = node.getMethod.getName.toString

  private[WalaInstructions] def getReceiverTypes(
    callInstr: SSAInvokeInstruction,
    node: CGNode
  ): Set[IClass] =
    getTypes(node, callInstr.getReceiver)

  def getTypes(node: CGNode, vn: ValueNumber): Set[IClass] = {
    val key = pointerAnalysis.getHeapModel.getPointerKeyForLocal(node, vn)
    (pointerAnalysis.getPointsToSet(key).asScala map {
      _.getConcreteType
    })(breakOut)
  }

  def getDeclaringClasses(callInstr: SSAInvokeInstruction, sourceNode: Node): Map[IClass, Set[IClass]] = {
    val targetSelector = new ClassHierarchyMethodTargetSelector(sourceNode.getMethod.getClassHierarchy)
    getReceiverTypes(callInstr, sourceNode.getNode).foldLeft(Map[IClass, Set[IClass]]() withDefaultValue Set.empty[IClass]) {
      case (result, receiverType) =>
        val targetMethod = targetSelector.getCalleeTarget(sourceNode.getNode, callInstr.getCallSite, receiverType)
        if (targetMethod == null)
          result
        else {
          val targetClass = targetMethod.getDeclaringClass
          result + (targetClass -> (result(targetClass) + receiverType))
        }
    }
  }
}
