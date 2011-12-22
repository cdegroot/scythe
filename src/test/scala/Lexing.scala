import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import util.parsing.input.CharSequenceReader

@RunWith(classOf[JUnitRunner])
class Lexing extends ThriftLexers with FlatSpec with ShouldMatchers {

  // Copied from http://henkelmann.eu/2011/01/29/an_introduction_to_scala_parser_combinators-part_3_unit_tests
  private def parsing[T](s: String)(implicit p: Parser[T]):T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)
    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,_) => throw new IllegalArgumentException(
                                     "Could not parse '" + s + "': " + msg)
    }
  }

  private def assertFail[T](input:String)(implicit p:Parser[T]) {
    evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
  }

  val they = it


  "The Thrift lexers" should "parse identifiers" in {
    implicit val parserToTest = identifier

    parsing("aValidIdentifier") should equal(Identifier("aValidIdentifier"))
    parsing("another_valid_identifier") should equal(Identifier("another_valid_identifier"))
    assertFail("an invalid identifier")
  }

  they should "parse string literals, both with single and double quotes" in {
    implicit val parserToTest = literal

    parsing("'a valid single quoted string'") should equal(StringLiteral("a valid single quoted string"))
    parsing("\"a valid double quoted string\"") should equal(StringLiteral("a valid double quoted string"))

    assertFail("'missing closing quote")
    assertFail("missing opening quote'")
  }

  they should "parse integer constants" in {
    implicit val parserToTest = intconstant

    parsing("0123") should equal(IntegerConstant(123))
    parsing("-42") should equal(IntegerConstant(-42))

    assertFail("a023")
    assertFail("2-1")
  }

  they should "parse double constants" in {
    implicit val parserToTest = doubleconstant

    parsing("1.23") should equal(DoubleConstant(1.23))
  }

  they should "parse constant lists" in {
    implicit val parserToTest = constlist

    parsing("[1, 2, 3]") should equal(ListConstant(List(IntegerConstant(1), IntegerConstant(2), IntegerConstant(3))))
  }
}