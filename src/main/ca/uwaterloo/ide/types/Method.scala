package ca.uwaterloo.ide.types

import com.ibm.wala.types.MethodReference

case class Method(
  name: String,
  parameterNum: Int,
  retType: String
) {

  def equalsUpToParamNum(any: Any): Boolean =
    any match {
      case Method(n, _, r) =>
        n == name && r == retType
      case _                  =>
        false
    }
}

object Method {

  def apply(method: MethodReference): Method =
    Method(
      method.getName.toString,
      method.getNumberOfParameters,
      method.getReturnType.getName.toString
    )
}