import parsing._
import util.parsing.input.CharSequenceReader

/*
 * Spike to see how code generation will look like.
 */


/**
 * Helper class to make sure we emit readable indentation.
 */
case class Indenter(sb: StringBuilder = new StringBuilder, level: Int = 0) {
  def more = Indenter(sb, level + 2)
  def less = Indenter(sb, level - 2)
  def append(s: Any): Indenter = { sb.append(s); this }
  def indent(s: Any): Indenter = { sb.append(" " * level).append(s); this }
  def newline: Indenter = { sb.append("\n"); this }

  def apply(f: (Indenter => Unit)) =  { f(this); this }

  override def toString = sb.toString
}

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

/**
 * Generates the service interface
 */
class ScalaServiceInterfaceCodeGen extends CodeGen {

  override def generateSource(node: Node,  indent: Indenter) {
    node match {
      case Document(header, definitions) =>
        definitions.map(generateSource(_, indent))

      case Service(name, base, defs) =>
        indent.indent("trait " + name + " {").newline
        defs.map { definition => generateSource(definition, indent.more); indent.newline }
        indent.indent("}").newline.newline

      case Function(t, n, args, throws, isOneWay) =>
        generateFunctionDef(Function(t, n, args, throws, isOneWay), indent)

      case _ => // ignore
    }
  }
}

/**
 * Generates the server-side implementation
 */
class ScalaServerImplementationCodeGen extends CodeGen {

  override def generateSource(node: Node,  indent: Indenter) {
    node match {
      case Document(header, definitions) =>
        definitions.map(generateSource(_, indent))

      case Service(name, base, defs) =>
        indent.indent("class " + name + "Impl extends " + name +" {").newline
        defs.map(generateSource(_, indent.more))
        indent.append("}").newline.newline

      case Function(t, n, args, throws, isOneWay) =>
        generateFunctionDef(Function(t, n, args, throws, isOneWay), indent)
        indent.append(" = {").newline
        indent.more { deeper =>
          deeper.indent("""// Method body here""").newline
          deeper.indent(""""Hello!"""").newline
        }
        indent.indent("}").newline

      case _ => // ignore
    }
  }
}

/**
 * Generates a simple server stub by combining all of the above.
 */
class SimpleServerStubGenerator
  extends ThriftParsers {

  def generate(source: String): String = {
    val phraseParser = phrase(document)
    val input = new CharSequenceReader(source)
    val ast: Document = phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,next) => throw new IllegalArgumentException(
                  "[" + next.pos + "] Could not parse: " + msg + "\n\n" + next.pos.longString)
    }

    val parts = List(new ScalaServiceInterfaceCodeGen, new ScalaServerImplementationCodeGen)
    parts.map(_.generateSource(ast)).reduceLeft((acc,part) => acc + part)
  }
}