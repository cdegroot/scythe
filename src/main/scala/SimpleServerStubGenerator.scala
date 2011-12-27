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
case class ScalaTopLevel(nodes: List[ScalaNode] = Nil) extends ScalaNode
case class Argument(name: String,  typeName: String)
case class ArgumentList(args: List[Argument]) extends ScalaNode
case class Trait(name: String, defs: List[TraitDef]) extends ScalaNode
case class TraitDef(name: String,  typeName: String,  args: ArgumentList) extends ScalaNode
case class TraitImpl(name: String, body: List[Method]) extends ScalaNode
case class Method(interface: TraitDef) extends ScalaNode

case class Indenter(sb: StringBuilder = new StringBuilder, level: Int = 0) {
  def more = Indenter(sb, level + 2)
  def less = Indenter(sb, level - 2)
  def append(s: Any): Indenter = { sb.append(s); this }
  def indent(s: Any): Indenter = { sb.append(" " * level).append(s); this }
  def newline: Indenter = { sb.append("\n"); this }

  def apply(f: (Indenter => Unit)) =  { f(this); this }

  override def toString = sb.toString
}


trait ScalaCodeOutput {

  def generateSource(node: ScalaNode): String = {
    val indent = new Indenter
    generateSource(node, indent)
    indent.toString
  }
  def generateSource(node: ScalaNode,  indent: Indenter) {
    node match {
      case ScalaTopLevel(nodes) =>
        nodes.map(generateSource(_, indent))

      case ArgumentList(args) =>
        indent.append(args.map { case Argument(name, typeName) =>
          name + ": " + typeName
        }.reduceLeft[String]{ (acc, part) => acc + ", " + part })

      case Trait(name, defs) =>
        indent.indent("trait " + name + " {").newline
        defs.map { definition => generateSource(definition, indent.more); indent.newline }
        indent.indent("}").newline.newline

      case TraitDef(name,  typeName,  args) =>
        indent.indent("def " + name + "(")
        generateSource(args, indent)
        indent.append("): ").append(typeName)

      case TraitImpl(name, body) =>
        indent.indent("class " + name + "Impl extends " + name +" {").newline
        body.map(generateSource(_, indent.more))
        indent.append("}").newline.newline

      case Method(interface) =>
        generateSource(interface,indent)
        indent.append(" = {").newline
        indent.more { deeper =>
          deeper.indent("""// Method body here""").newline
          deeper.indent(""""Hello!"""").newline
        }
        indent.indent("}").newline
    }
  }
}

/**
 * Generate a simple server stub.
 */
class SimpleServerStubGenerator
  extends ThriftParsers with ScalaCodeOutput {

  def generate(source: String): String = {
    val phraseParser = phrase(document)
    val input = new CharSequenceReader(source)
    val ast: Document = phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,next) => throw new IllegalArgumentException(
                  "[" + next.pos + "] Could not parse: " + msg + "\n\n" + next.pos.longString)
    }

    generateSource(generateCodeFromAst(ast: Document))
  }

  def generateCodeFromAst(ast: Document): ScalaTopLevel = new ScalaTopLevel(ast.definitions.flatMap(generateDefinitions(_)))


  def generateDefinitions(definition: ToplevelNode) = definition match {
      case service: Service => generateService(service)
  }

  def generateService(service: Service) : List[ScalaNode] =
    List(generateInterface(service: Service), generateImplementation(service: Service))

  def generateInterface(service: Service) = Trait(service.n, service.functions.map(generateInterfaceMethod(_)))

  def generateInterfaceMethod(function: Function) = TraitDef(function.n,  typeToScala(function.t),
    ArgumentList(function.args.map( arg => Argument(arg.n, typeToScala(arg.t)))))

  def generateImplementation(service: Service) = TraitImpl(service.n,  service.functions.map(generateImplementationMethod(_)))

  def generateImplementationMethod(function: Function) = Method(generateInterfaceMethod(function))

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