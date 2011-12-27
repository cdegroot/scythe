package parsing

import util.parsing.input.CharSequenceReader

/**
 * Wrapper around the whole parsing stuff. This should be the only stuff visible outside the
 * "parsing" package.
 */
object ThriftParser extends ThriftParsers {

  /** Parse a source to an AST */
  def parseToAst(source: String): ThriftAst.Document = {
    val phraseParser = phrase(document)
    val input = new CharSequenceReader(source)
    phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,next) => throw new IllegalArgumentException(
                  "[" + next.pos + "] Could not parse: " + msg + "\n\n" + next.pos.longString)
    }
  }
}

object ThriftAst {
  // Thrift tokens and nodes, collectively something you can call an AST

  sealed abstract class Token
  abstract class Constant extends Token
  case class Identifier(name: String) extends Constant
  case class StringConstant(value: String) extends Constant
  case class IntegerConstant(value: Int) extends Constant
  case class DoubleConstant(value: Double) extends Constant
  case class ListConstant(value: List[Constant]) extends Constant
  case class MapConstant(value: Map[Constant,Constant]) extends Constant

  sealed abstract class Node
  abstract class Type extends Node
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

  abstract class ToplevelNode extends Node
  case class Service(n: String,  baseService: Option[String],  functions: List[Function]) extends ToplevelNode
  case class Exception(n: String, args: List[Field]) extends ToplevelNode
  case class Struct(n: String,  args: List[Field]) extends ToplevelNode
  case class Enum(n: String,  elems: List[EnumElem]) extends ToplevelNode
  case class EnumElem(n: String,  value: Option[Int]) extends Node
  case class Typedef(n: String,  t: Type) extends ToplevelNode
  case class Const(n: String,  t: Type,  value: Constant) extends ToplevelNode
  abstract class HeaderNode extends Node
  case class Namespace(scope: String,  identifier: String) extends HeaderNode
  case class Include(n: String) extends HeaderNode
  case class CppInclude(n: String) extends HeaderNode
  case class Header(elems: List[HeaderNode]) extends Node

  case class Document(header: Header, definitions: List[ToplevelNode]) extends Node

}