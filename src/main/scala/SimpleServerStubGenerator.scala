import parsing._
import util.parsing.input.CharSequenceReader

/**
 * Spike to see how code generation will look like. If you think this here duplicates
 * stuff from the scala compiler: it does. That stuff is just too complex to easily
 * reuse, IMNSHO.... We won't probably go as far as building a fully covering
 * structure for any Scala program, the idea is that this is a thrift generator
 * so sooner or later we'll emit Thrift specific things here in one go :)
 */

abstract class ScalaNode
case class ScalaTopLevelNode(nodes: List[ScalaNode] = Nil) extends ScalaNode
case class Argument(name: String,  typeName: String)
case class ArgumentList(args: List[Argument]) extends ScalaNode
case class ScalaService(name: String, defs: List[ScalaFunction]) extends ScalaNode
case class ScalaFunction(name: String,  typeName: String,  args: ArgumentList) extends ScalaNode

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
  def generateSource(node: ScalaNode): String = {
    val indent = new Indenter
    generateSource(node, indent)
    indent.toString
  }
  def generateSource(node: ScalaNode,  indent: Indenter)

  // Common helpers

  def generateFunctionDef(function: ScalaFunction,  indent: Indenter) {
    indent.indent("def " + function.name + "(")
    generateArgumentList(function.args, indent)
    indent.append("): ").append(function.typeName)
  }

  def generateArgumentList(argumentList: ArgumentList,  indent: Indenter) {
    indent.append(argumentList.args.map { case Argument(name, typeName) =>
          name + ": " + typeName
    }.reduceLeft[String]{ (acc, part) => acc + ", " + part })
  }
}

/**
 * Generates the service interface
 */
class ScalaServiceInterfaceCodeGen extends CodeGen {

  override def generateSource(node: ScalaNode,  indent: Indenter) {
    node match {
      case ScalaTopLevelNode(nodes) =>
        nodes.map(generateSource(_, indent))

      case ScalaService(name, defs) =>
        indent.indent("trait " + name + " {").newline
        defs.map { definition => generateSource(definition, indent.more); indent.newline }
        indent.indent("}").newline.newline

      case ScalaFunction(name,  typeName,  args) =>
        generateFunctionDef(ScalaFunction(name,  typeName,  args), indent)

      case _ => // ignore
    }
  }
}

/**
 * Generates the server-side implementation
 */
class ScalaServerImplementationCodeGen extends CodeGen {

  override def generateSource(node: ScalaNode,  indent: Indenter) {
    node match {
      case ScalaTopLevelNode(nodes) =>
        nodes.map(generateSource(_, indent))

      case ScalaService(name, defs) =>
        indent.indent("class " + name + "Impl extends " + name +" {").newline
        defs.map(generateSource(_, indent.more))
        indent.append("}").newline.newline

      case ScalaFunction(name,  typeName,  args) =>
        generateFunctionDef(ScalaFunction(name,  typeName,  args), indent)
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
 * Generate a simple server stub.
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

    val tree = generateCodeFromAst(ast: Document)
    val parts = List(new ScalaServiceInterfaceCodeGen(), new ScalaServerImplementationCodeGen)
    parts.map(_.generateSource(tree)).reduceLeft((acc,part) => acc + part)
  }

  def generateCodeFromAst(ast: Document): ScalaTopLevelNode = new ScalaTopLevelNode(ast.definitions.map(generateDefinitions(_)))

  def generateDefinitions(definition: ToplevelNode) = definition match {
      case service: Service => generateService(service)
  }

  def generateService(service: Service) = ScalaService(service.n, service.functions.map(generateInterfaceMethod(_)))

  def generateInterfaceMethod(function: Function) = ScalaFunction(function.n,  typeToScala(function.t),
    ArgumentList(function.args.map( arg => Argument(arg.n, typeToScala(arg.t)))))

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