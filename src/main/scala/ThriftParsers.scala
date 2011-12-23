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
object VoidType extends Type
case class ListType(t: Type) extends Type
case class SetType(t: Type) extends Type
case class MapType(keyType: Type,  valueType: Type) extends Type
case class ComplexType(t: String) extends Type

case class Field(t: Type,  n: String, id: Option[IntegerConstant], required: Boolean) extends Node
case class Function(t: Type,  n: String,  args: List[Field], throws: List[Field], isOneway: Boolean) extends Node

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

    def field : Parser[Field] = opt(fieldid) ~ opt(fieldreq) ~ fieldtype ~ identifier ^^ {
      case fid~freq~ftype~fname =>
        val required = freq.getOrElse(false)
        new Field(ftype, fname.name, fid, required) }

    def fieldid: Parser[IntegerConstant] = intconstant <~ ":"

    def fieldreq: Parser[Boolean] = "required" ^^^ { true } | "optional" ^^^ { false }

    def function: Parser[Function] = functionOneWay ~ functiontype ~ identifier ~ "(" ~ repsep(field, listseparator) ~ ")" ~ opt(throws) ^^ {
      case fow~ftype~fid~"("~fields~")"~throwsOpt =>
        val throwsList = throwsOpt.getOrElse(List())
        new Function(ftype, fid.name, fields, throwsList, fow)
    }

    def functionOneWay: Parser[Boolean] = opt("oneway") ^^ {
      case Some(x) => true
      case None => false }

    def functiontype: Parser[Type] =  "void" ^^^ { VoidType } | fieldtype

    def throws: Parser[List[Field]] = "throws" ~ "(" ~> repsep(field, listseparator) <~ ")" ^^ { case list => list }

    def fieldtype: Parser[Type] = basetype | containertype | identifier ^^ { case id => ComplexType(id.name)}

    def definitiontype = basetype | containertype

    def basetype: Parser[Type] = {
      "bool" ^^^ { BoolType } | "byte" ^^^ { ByteType } | "i16" ^^^ { Int16Type } |
      "i32" ^^^ { Int32Type } | "i64" ^^^ { Int64Type } | "double" ^^^ { DoubleType } |
      "string" ^^^ { StringType } | "binary" ^^^ { BinaryType } | "slist" ^^^ { SlistType }
    }
  
    def containertype = maptype | settype | listtype

    def maptype: Parser[MapType] = (("map" ~ opt(cpptype) ~ '<' ~> fieldtype <~ ',') ~ fieldtype <~ '>') ^^
      { case keyType~valueType => new MapType(keyType, valueType) }

    def settype: Parser[SetType] = ("set" ~ opt(cpptype) ~ '<' ~> fieldtype <~ '>') ^^ { new SetType(_) }

    def listtype: Parser[ListType] = ("list" ~ '<' ~> fieldtype <~ '>' ~ opt(cpptype)) ^^ { new ListType(_) }

    // Defined for compatibility, but unused
    def cpptype: Parser[Any] = "cpp_type" ~ literal
}

