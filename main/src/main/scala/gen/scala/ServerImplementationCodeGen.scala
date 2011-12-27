package gen.scala

import gen.util.Indenter
import parsing.ThriftAst._

/**
 * Generates the service interface
 */
class ServerImplementationCodeGen extends CodeGen {

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
