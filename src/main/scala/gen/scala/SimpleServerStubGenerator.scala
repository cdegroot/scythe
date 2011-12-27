package gen.scala

import parsing._
import util.parsing.input.CharSequenceReader

/**
 * Generates a simple server stub by combining all of the above.
 */
class SimpleServerStubGenerator
  {

  def generate(source: String): String = {
    val ast: Document = Parser.parseToAst(source)
    val parts = List(new ScalaServiceInterfaceCodeGen, new ServerImplementationCodeGen)
    parts.map(_.generateSource(ast)).reduceLeft((acc,part) => acc + part)
  }
}