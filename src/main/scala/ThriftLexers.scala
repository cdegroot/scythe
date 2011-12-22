import util.parsing.combinator.RegexParsers

sealed abstract class Token
case class Identifier(name: String) extends Token
case class StringLiteral(value: String) extends Token
case class IntegerConstant(value: Int) extends Token
case class DoubleConstant(value: Double) extends Token

/**
 * Lexical part of thrift IDL parsing.
 */
trait ThriftLexers extends RegexParsers {
//
//  def constvalue = intconstant | doubleconstant | listeral | identifier | constlist | constmap
//
  def intconstant = regex("[+-]?[0-9]+"r) ^^ { case s: String => new IntegerConstant(s.toInt) }

  def doubleconstant = regex("[+-]?[0-9]+(.?[0-9]+)?([Ee][0-9]+)?"r) ^^ { case s: String => new DoubleConstant(s.toDouble) }

//
//  def constlist = "[" ~ (constvalue ~ listseparator?)* ~ "]"
//
//  def constmap = "{" ~ (constvalue ~ ":" ~ constvalue ~ listseparator?)* ~ "}"
//

  def literal = (( '\"' ~>  regex("[^\"]*"r) <~ '\"' ) | ( '\'' ~> regex("[^']*"r) <~ '\'' )) ^^ { new StringLiteral(_) }

  def identifier = regex("[a-zA-Z_][a-zA-Z0-0._]*"r) ^^ { new Identifier(_) }

  // For Thrift compatibility, but we don't act on it
  def stidentifier = regex("[a-zA-Z_][a-zA-Z0-0._-]*"r)

  // def listseparator = "," | ";"

}

