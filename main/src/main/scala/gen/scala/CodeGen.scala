package gen.scala

import gen.util.Indenter
import parsing.ThriftAst._

/**
 * Base class for code generation. Contains a (hopefully) common interface and
 * some utilities.
 */
trait CodeGen {
  def generateSource(node: Document): String = {
    val indent = new Indenter
    generateSource(node, indent)
    indent.toString
  }
  def generateSource(node: Node,  indent: Indenter)

  // Common helpers

  def generateFunctionDef(function: Function,  indent: Indenter) {
    indent.indent("def " + function.n + "(")
    generateArgumentList(function.args, indent)
    indent.append("): ").append(typeToScala(function.t))
  }

  def generateArgumentList(argumentList: List[Field],  indent: Indenter) {
    indent.append(argumentList.map { arg =>
      arg.n + ": " + typeToScala(arg.t)
    }.reduceLeft[String]{ (acc, part) => acc + ", " + part })
  }

  def typeToScala(t: Type): String = t match {
    case BoolType => "Boolean"
    case ByteType => "Byte"
    case Int16Type => "Short"
    case Int32Type => "Integer"
    case Int64Type => "Long"
    case DoubleType => "Double"
    case StringType => "String"
    case BinaryType => "Array[Byte]"
    case VoidType => "None"
    case t: ListType => "List[" + typeToScala(t.t) + "]"
    case t: SetType => "Set[" + typeToScala(t.t) + "]"
    case t: MapType => "Map[" + typeToScala(t.keyType) + ", " + typeToScala(t.valueType) + "]"
    case t: ComplexType => t.t
  }
}
