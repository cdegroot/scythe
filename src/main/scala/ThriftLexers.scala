import util.parsing.combinator.RegexParsers

sealed abstract class ThriftLexerNode
case class Identifier(name: String) extends ThriftLexerNode

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
//  def literal: Parser[String] = ( "\"" ~ "[^\"]*"r ~ "\"" ) | ( "'" ~ "[^']*"r ~ "'" )

  def identifier = regex("[a-zA-Z_][a-zA-Z0-0._]*"r) ^^ { case id => new Identifier(id) }

  // def stidentifier = (letter | "_") ~ (letter | digit | "." | "_" | "-")*

  // def listseparator = "," | ";"

  // def letter = "[A-Za-z]"r

  // def digit = "[0-9]"r

}

