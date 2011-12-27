import parsing._
import util.parsing.input.CharSequenceReader

/**
 * Spike to see how code generation will look like. If you think this here duplicates
 * stuff from the scala compiler: it does. That stuff is just too complex to easily
 * reuse, IMNSHO.... We won't probably go as far as building a fully covering
 * structure for any Scala program, the idea is that this is a thrift generator
 * so sooner or later we'll emit Thrift specific things here in one go :)
 */

/** A little class that indents for you */
case class Indenter(sb: StringBuilder = new StringBuilder, level: Int = 0) {
  def more = Indenter(sb, level + 2)
  def less = Indenter(sb, level - 2)
  def append(s: Any): Indenter = { sb.append(s); this }
  def indent(s: Any): Indenter = { sb.append(" " * level).append(s); this }
  override def toString = sb.toString
}

abstract class ScalaNode {
  def generateSource(indent: Indenter)

}
case class ScalaTopLevel(nodes: List[ScalaNode] = Nil) extends ScalaNode {
  def generateSource: String = {
    val indent = new Indenter
    generateSource(indent)
    indent.toString
  }
  def generateSource(indent: Indenter) {
    nodes.map(_.generateSource(indent))
  }
}
case class Trait(name: String, defs: List[TraitDef]) extends ScalaNode {
  def generateSource(indent: Indenter) {
    indent.indent("trait " + name + " {\n")
    defs.map(_.generateSource(indent.more))
    indent.indent("}")
  }
}
case class Argument(name: String,  typeName: String)
case class ArgumentList(args: List[Argument]) {
  def generateSource(indent: Indenter) {
    indent.append(args.map { case Argument(name, typeName) =>
      name + ": " + typeName
    }.reduceLeft[String]{ (acc, part) => acc + ", " + part })
  }
}
case class TraitDef(name: String,  typeName: String,  args: ArgumentList) extends ScalaNode {
  def generateSource(indent: Indenter) {
    indent.indent("def " + name + "(")
    args.generateSource(indent)
    indent.append("): ").append(typeName).append("\n")
  }
}

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

    generateCodeFromAst(ast: Document).generateSource
  }

  def generateCodeFromAst(ast: Document): ScalaTopLevel = new ScalaTopLevel(ast.definitions.map(generateDefinitions(_)))


  def generateDefinitions(definition: ToplevelNode) = definition match {
      case service: Service => generateService(service)
  }

  def generateService(service: Service) = generateInterface(service: Service)

  def generateInterface(service: Service) = Trait(service.n, service.functions.map(generateInterfaceMethod(_)))

  def generateInterfaceMethod(function: Function) = TraitDef(function.n,  typeToScala(function.t),
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