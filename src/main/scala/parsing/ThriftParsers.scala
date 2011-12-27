package parsing

import util.parsing.combinator.RegexParsers

sealed abstract class Node
abstract class Type extends Node
// TSTTCPW - we use type objects instead of classes for now. We'll see how that works
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

abstract class ToplevelNode extends Node // Mostly there for documentation
case class Service(n: String,  baseService: Option[String],  functions: List[Function]) extends ToplevelNode
case class Exception(n: String, args: List[Field]) extends ToplevelNode
case class Struct(n: String,  args: List[Field]) extends ToplevelNode
case class Enum(n: String,  elems: List[EnumElem]) extends ToplevelNode
case class EnumElem(n: String,  value: Option[Int]) extends Node
case class Typedef(n: String,  t: Type) extends ToplevelNode
case class Const(n: String,  t: Type,  value: Constant) extends ToplevelNode

abstract class HeaderNode extends Node
// Simple modeling of namespaces for now, can always be extended.
case class Namespace(scope: String,  identifier: String) extends HeaderNode
case class Include(n: String) extends HeaderNode
case class CppInclude(n: String) extends HeaderNode
case class Header(elems: List[HeaderNode]) extends Node

case class Document(header: Header, definitions: List[ToplevelNode]) extends Node


trait ThriftParsers extends RegexParsers with ThriftLexers {
  // Strip comments for now. TODO[cdg] Ideally, they get attached to the AST in a later stage
  // so you can directly generate documentation from it
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  // note: we left the production rule names the same as in the Thrift IDL for easy reference

  // Parser from the Thrift IDL description page minus the Facebook xsd internal crap
  def document: Parser[Document] = header ~ zeroOrMoreOf(definition) ^^ { case header~body => new Document(header, body) }

  def header: Parser[Header] = rep(includeFile | cppincludeFile | namespace) ^^ { case stuff => Header(stuff) }

    // TODO[cdg] beats me why we have to be explicit about whitespace here...
  def includeFile: Parser[Include] = regex("include\\s+"r) ~> literal ^^ { case lit => Include(lit.value)}

  def cppincludeFile: Parser[CppInclude] = regex("cpp_include\\s+".r) ~> literal ^^ { case lit => CppInclude(lit.value)}

  def namespace: Parser[Namespace] = {
    ( "namespace" ~> ( namespacescope ~ identifier ^^ { case scope~id => Namespace(scope, id.name) }
                     | "smaltalk.category" ~ stidentifier ^^ { case scope~id => Namespace(scope, id) }
                     | "smalltalk.prefix" ~ identifier ^^ { case scope~id => Namespace(scope, id.name) }
                     )) |
    "php_namespace" ~> literal ^^ { case id => Namespace("php", id.value) } |
    "xsd_namespace" ~> literal ^^ { case id => Namespace("xsd", id.value) }
  }

  def namespacescope: Parser[String] = "*" | "cpp" | "java" | "py" | "perl" | "rb" | "cocoa" | "csharp"

  def definition: Parser[ToplevelNode] = const | typedef | enum | struct | exception | service

  def const: Parser[Const] = "const" ~> fieldtype ~ identifier ~ "=" ~ constvalue ^^ {
    case ftype~id~"="~value => new Const(id.name, ftype, value)
  }

  def typedef: Parser[Typedef] = "typedef" ~> definitiontype ~ identifier ^^ {
    case deftype~id => new Typedef(id.name, deftype)
  }

  def enum: Parser[Enum] = "enum" ~> identifier ~ "{" ~ zeroOrMoreOf(enumElem) ~ "}" ^^ {
    case id~"{"~elems~"}" => new Enum(id.name, elems)
  }

  def enumElem: Parser[EnumElem] = identifier ~ opt("=" ~> intconstant) ^^ {
    case id~constOpt => new EnumElem(id.name, constOpt.flatMap(const => Some(const.value)))
  }

  // what is senum? Ignore :)
  //def senum: Parser[Any] = "senum" ~ identifier ~ "{" ~ zeroOrMoreOf(literal) ~ "}"

  def struct: Parser[Struct] = "struct" ~> identifier ~  "{" ~ zeroOrMoreOf(field) ~ "}" ^^ {
    case id~"{"~fields~"}" => new Struct(id.name, fields)
  }

  def exception: Parser[Exception] = "exception" ~> identifier ~ "{" ~ zeroOrMoreOf(field) ~ "}" ^^ {
    case id~"{"~fields~"}" => new Exception(id.name, fields)
  }

  def service: Parser[Service] = "service" ~ identifier ~ opt("extends" ~> identifier) ~ "{" ~ zeroOrMoreOf(function) ~ "}" ^^ {
    case "service"~sname~extendsOpt~"{"~funlist~"}" =>
      val extendsName = extendsOpt.flatMap(id => Some(id.name))
      new Service(sname.name, extendsName, funlist)
  }


  def field: Parser[Field] = opt(fieldid) ~ opt(fieldreq) ~ fieldtype ~ identifier ^^ {
    case fid~freq~ftype~fname =>
      val required = freq.getOrElse(false)
      new Field(ftype, fname.name, fid, required) }

  def fieldid: Parser[IntegerConstant] = intconstant <~ ":"

  def fieldreq: Parser[Boolean] = "required" ^^^ { true } | "optional" ^^^ { false }

  def function: Parser[Function] = functionOneWay ~ functiontype ~ identifier ~ "(" ~ zeroOrMoreOf(field) ~ ")" ~ opt(throws) ^^ {
    case fow~ftype~fid~"("~fields~")"~throwsOpt =>
      val throwsList = throwsOpt.getOrElse(List())
      new Function(ftype, fid.name, fields, throwsList, fow)
  }

  def functionOneWay: Parser[Boolean] = opt("oneway") ^^ {
    case Some(x) => true
    case None => false }

  def functiontype: Parser[Type] =  "void" ^^^ { VoidType } | fieldtype

  def throws: Parser[List[Field]] = "throws" ~ "(" ~> zeroOrMoreOf(field) <~ ")" ^^ { case list => list }

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

  // Utilities
  def zeroOrMoreOf[T](p: Parser[T]): Parser[List[T]] = repsep(p, listseparator)
}

