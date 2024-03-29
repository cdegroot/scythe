package parsing

import util.parsing.combinator.RegexParsers
import ThriftAst._

/**
 * Lexical part of thrift IDL parsing. Not really a lexer, this is just the low
 * level bit of the whole parser, mostly separated out to make code and test source
 * shorter
 */
private[parsing] trait ThriftLexers extends RegexParsers {

  // note: we left the production rule names the same as in the Thrift IDL for easy reference. We do
  // clean naming a bit in the Token hierarchy we return.

  def constvalue : Parser[Constant] = intconstant | doubleconstant | literal | identifier | constlist | constmap

  def intconstant = regex("[+-]?[0-9]+"r) ^^ { case s: String => new IntegerConstant(s.toInt) }

  def doubleconstant = regex("[+-]?[0-9]+(.?[0-9]+)?([Ee][0-9]+)?"r) ^^ { case s: String => new DoubleConstant(s.toDouble) }

  def constlist : Parser[ListConstant] = '[' ~> repsep(constvalue, listseparator) <~ ']' ^^ { new ListConstant(_) }

  def constmap = '{' ~> repsep(constmapelem, listseparator?) <~ '}' ^^ { case l: List[(Constant,Constant)] => new MapConstant(Map(l: _* )) }

  def constmapelem : Parser[(Constant,Constant)] = (constvalue <~ ':') ~ constvalue ^^ { case key~value => (key, value) }

  def literal = (( '\"' ~>  regex("[^\"]*"r) <~ '\"' ) | ( '\'' ~> regex("[^']*"r) <~ '\'' )) ^^ { new StringConstant(_) }

  def identifier = regex("[a-zA-Z_][a-zA-Z0-9._]*"r) ^^ { new Identifier(_) }

  // For Thrift compatibility, but we don't act on it
  def stidentifier = regex("[a-zA-Z_][a-zA-Z0-9._-]*"r)

  def listseparator  = elem(',') | elem(';')

}

