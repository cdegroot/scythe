package gen.scala

import parsing.ThriftAst._
import parsing.ThriftParser
import util.parsing.input.CharSequenceReader

/**
 * Generates a simple server stub by combining all of the above.
 */
class SimpleServerStubGenerator
  {

  def generate(source: String): String = {
    val ast: Document = ThriftParser.parseToAst(source)
    val parts = List(new ScalaServiceInterfaceCodeGen, new ServerImplementationCodeGen)
    parts.map(_.generateSource(ast)).reduceLeft((acc,part) => acc + part)
  }
}