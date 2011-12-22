import util.parsing.combinator.RegexParsers

sealed abstract class Token
abstract class Constant extends Token
case class Identifier(name: String) extends Constant
case class StringLiteral(value: String) extends Constant
case class IntegerConstant(value: Int) extends Constant
case class DoubleConstant(value: Double) extends Constant
case class ListConstant(value: List[Constant]) extends Constant

/**
 * Lexical part of thrift IDL parsing.
 */
trait ThriftLexers extends RegexParsers {

  def constvalue : Parser[Constant] = intconstant | doubleconstant | literal | identifier | constlist // | constmap

  def intconstant = regex("[+-]?[0-9]+"r) ^^ { case s: String => new IntegerConstant(s.toInt) }

  def doubleconstant = regex("[+-]?[0-9]+(.?[0-9]+)?([Ee][0-9]+)?"r) ^^ { case s: String => new DoubleConstant(s.toDouble) }

  def constlist : Parser[ListConstant] = '[' ~> repsep(constvalue, listseparator) <~ ']' ^^ { new ListConstant(_) }

//
//  def constmap = "{" ~ (constvalue ~ ":" ~ constvalue ~ listseparator?)* ~ "}"
//

  def literal = (( '\"' ~>  regex("[^\"]*"r) <~ '\"' ) | ( '\'' ~> regex("[^']*"r) <~ '\'' )) ^^ { new StringLiteral(_) }

  def identifier = regex("[a-zA-Z_][a-zA-Z0-0._]*"r) ^^ { new Identifier(_) }

  // For Thrift compatibility, but we don't act on it
  def stidentifier = regex("[a-zA-Z_][a-zA-Z0-0._-]*"r)

  def listseparator  = elem(',') | elem(';')

}

