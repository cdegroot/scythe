import util.parsing.combinator.RegexParsers

sealed abstract class Token
case class Identifier(name: String) extends Token
case class StringLiteral(value: String) extends Token

/**
 * Lexical part of thrift IDL parsing.
 */
trait ThriftLexers extends RegexParsers {
//
//  def constvalue = intconstant | doubleconstant | listeral | identifier | constlist | constmap
//
//  def intconstant = ("+" | "-")? ~ digit+
//
//  def doubleconstant = ("+" | "-")? ~ digit+ ~ ("." ~ digit+)? ~ (("E" | "e") ~ intconstant)?
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

