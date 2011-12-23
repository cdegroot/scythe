import parsing._
import util.parsing.input.CharSequenceReader

/**
 * Spike to see how code generation will look like
 */
class SimpleServerStubGenerator extends ThriftParsers {
  // TODO[cdg] automatic indentation
  // TODO[cdg] implicit passing of string builder, indentation level, ...?
  // TODO[cdg] less mixing of strings and code calls?
  def generate(source: String): String = {
    val phraseParser = phrase(document)
    val input = new CharSequenceReader(source)
    val ast = phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,next) => throw new IllegalArgumentException(
                  "[" + next.pos + "] Could not parse: " + msg + "\n\n" + next.pos.longString)
    }

    generateFromAst(ast)
  }

  def generateFromAst(ast: Document) = {
    val sb = new StringBuilder
    ast.definitions.map(generateDefinitions(_, sb))
    sb.toString
  }

  def generateDefinitions(definition: ToplevelNode, sb: StringBuilder) {
    definition match {
      case service: Service => generateService(service, sb)
    }
  }

  def generateService(service: Service,  sb: StringBuilder) {
    generateInterface(service: Service,  sb: StringBuilder)
  }

  def generateInterface(service: Service,  sb: StringBuilder) {
    sb.append("trait " + service.n + " {\n")
    service.functions.map(generateInterfaceMethod(_, sb))
    sb.append("\n}")
  }

  def generateInterfaceMethod(function: Function, sb: StringBuilder) {
    sb.append("  def " + function.n + "(")
    sb.append(function.args.map { arg =>
      arg.n + ": " + typeToScala(arg.t)
    }.reduceLeft[String]{ (acc, part) => acc + ", " + part })
    sb.append("): ").append(typeToScala(function.t))
  }

  def typeToScala(t: Type): String = {
    t match {
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
}