import util.parsing.combinator.RegexParsers

trait ThriftParsers extends RegexParsers with ThriftLexers {

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
//  def fieldtype = identifier | basetype | containertype
//
//  def definitiontype = basetype | containertype
//
//  def basetype = "bool" | "byte" | "i16" | "i32" | "i64" | "double" | "string" | "binary" | "slist"
//
//  def containertype = maptype | settype | listtype
//
//  def maptype = "map" ~ cpptype? ~ "<" ~ fieldtype ~ "," ~ fieldtype ~ ">"
//
//  def settype = "set" ~ cpptype? ~ "<" ~ fieldtype ~ ">"
//
//  def listtype = "list" ~ "<" ~ fieldtype ~ ">" ~ cpptype?
//
//  def cpptype = "cpp_type" ~ literal
}

