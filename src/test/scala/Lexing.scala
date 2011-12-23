import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import util.parsing.input.CharSequenceReader

class Lexing extends FlatSpecForParsers with ThriftLexers {

  "The Thrift lexers" should "parse identifiers" in {
    implicit val parserToTest = identifier

    parsing("aValidIdentifier") should equal(Identifier("aValidIdentifier"))
    parsing("another_valid_identifier") should equal(Identifier("another_valid_identifier"))
    parsing("alsoValid32") should equal(Identifier("alsoValid32"))
    assertFail("an invalid identifier")
  }

  they should "parse string literals, both with single and double quotes" in {
    implicit val parserToTest = literal

    parsing("'a valid single quoted string'") should equal(StringConstant("a valid single quoted string"))
    parsing("\"a valid double quoted string\"") should equal(StringConstant("a valid double quoted string"))

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

  they should "parse constant maps" in {
    implicit val parserToTest = constmap

    val expectedList = List((1, "foo"), (2, "bar"))
    val expectedConstantsList = expectedList.map(t => (IntegerConstant(t._1), StringConstant(t._2)))
    val expectedConstantMap : Map[Constant,Constant] = Map(expectedConstantsList : _*)
    parsing("{1:'foo', 2:'bar'}") should equal(MapConstant(expectedConstantMap))
  }
}