package parsing

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import util.parsing.combinator.{RegexParsers, Parsers}
import util.parsing.input.CharSequenceReader

@RunWith(classOf[JUnitRunner])
class FlatSpecForParsers extends FlatSpec with ShouldMatchers {
  parser : RegexParsers =>

  // Copied from http://henkelmann.eu/2011/01/29/an_introduction_to_scala_parser_combinators-part_3_unit_tests
  def parsing[T](s: String)(implicit p: Parser[T]):T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)

    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,next) => throw new IllegalArgumentException(
                  "[" + next.pos + "] Could not parse: " + msg + "\n\n" + next.pos.longString)
    }
  }

  def assertFail[T](input:String)(implicit p:Parser[T]) {
    evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
  }

  val they = it


}