package parsing

import util.parsing.input.CharSequenceReader

/**
 * Wrapper around the whole parsing stuff. This should be the only stuff visible outside the
 * "parsing" package.
 */
object Parser extends ThriftParsers {
  def parseToAst(source: String): Document = {
    val phraseParser = phrase(document)
    val input = new CharSequenceReader(source)
    phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,next) => throw new IllegalArgumentException(
                  "[" + next.pos + "] Could not parse: " + msg + "\n\n" + next.pos.longString)
    }
  }
}