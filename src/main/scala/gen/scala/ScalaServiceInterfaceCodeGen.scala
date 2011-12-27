package gen.scala

import gen.util.Indenter
import parsing._

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
