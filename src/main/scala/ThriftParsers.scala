import util.parsing.combinator.RegexParsers

sealed abstract class Node
abstract class Type extends Node
// TSTTCPW - we use type objects instead of classes for now.
object BoolType extends Type
object ByteType extends Type
object Int16Type extends Type
object Int32Type extends Type
object Int64Type extends Type
object DoubleType extends Type
object StringType extends Type
object BinaryType extends Type
object SlistType extends Type
case class ListType(t: Type) extends Type
case class SetType(t: Type) extends Type
case class MapType(keyType: Type,  valueType: Type) extends Type
case class ComplexType(t: Identifier) extends Type

trait ThriftParsers extends RegexParsers with ThriftLexers {
  // note: we left the production rule names the same as in the Thrift IDL for easy reference

  // Parser from the Thrift IDL description page minus the Facebook xsd internal crap
//  def document = header* ~ definition*
//
//  def header = (include | cppinclude | namespace)
//
//  def include = "include" ~ literal
//
//  def cppinclude = "cpp_include" ~ literal
//
//  def namespace = (
//    "namespace" ~ ( namespacescope ~ identifier | "smaltalk.category" ~ stidentifier | "smalltalk.prefix" ~ identifier ) |
//    "php_namespace" ~ literal  |
//    "xsd_namespace" ~ literal
//  )
//
//  def namespacescope = "*" | "cpp" | "java" | "py" | "perl" | "rb" | "cocoa" | "csharp"
//
//  def definition = const | typedef | enum | senum | struct | exception | service
//
//  def const = "const" ~ fieldtype ~ identifier ~ "=" ~ constvalue ~ listseparator?
//
//  def typedef = "typedef" ~ definitiontype ~ identifier
//
//  def enum = "enum" ~ identifier ~ "{" ~ (identifier ~ ("=" ~ intconstant)? ~ listseparator?)* ~ "}"
//
//  def senum = "senum" ~ identifier ~ "{" ~ (literal ~ listseparator?)* ~ "}"
//
//  def struct = "struct" ~ identifier ~  "{" ~ field* ~ "}"
//
//  def exception = "exception" ~ identifier ~ "{" ~ field* ~ "}"
//
//  def service = "service" ~ identifier ~ ( "extends" ~ identifier)? ~ "{" ~ function* ~ "}"
//
//  def field = fieldid? ~ fieldreq? ~ fieldtype ~ identifier ~ ("=" ~ constvalue)? ~ listseparator?
//
//  def fieldid = intconstant ~ ":"
//
//  def fieldreq = "required" | "optional"
//
//  def function = "oneway"? ~ functiontype ~ identifier ~ "(" ~ field* ~ ")" ~ throws? ~ listseparator
//
//  def functiontype = fieldtype | "void"
//
//  def throws = "throws" ~ "(" ~ field* ~ ")"
//
    def fieldtype: Parser[Type] = basetype | containertype | identifier ^^ { ComplexType(_)}

    def definitiontype = basetype | containertype

    def basetype = {
      literal("bool") ^^^ { BoolType } | literal("byte") ^^^ { ByteType } | literal("i16") ^^^ { Int16Type } |
      literal("i32") ^^^ { Int32Type } | literal("i64") ^^^ { Int64Type } | literal("double") ^^^ { DoubleType } |
      literal("string") ^^^ { StringType } | literal("binary") ^^^ { BinaryType } | literal("slist") ^^^ { SlistType }
    }
  
    def containertype = maptype | settype | listtype

    def maptype = ((literal("map") ~ opt(cpptype) ~ elem('<') ~> fieldtype <~ elem(',')) ~ fieldtype <~ elem('>')) ^^
      { case v => new MapType(v._1, v._2) }

    def settype = (literal("set") ~ opt(cpptype) ~ elem('<') ~> fieldtype <~ elem('>')) ^^ { new SetType(_) }

    def listtype = (literal("list") ~ elem('<') ~> fieldtype <~ elem('>') ~ opt(cpptype)) ^^ { new ListType(_) }

    // Defined for compatibility, but unused
    def cpptype = literal("cpp_type") ~ literal
}

