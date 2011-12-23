
class Parsing extends FlatSpecForParsers with ThriftParsers {

  "The Thrift parsers" should "parse a list definition" in {
    implicit val parserToTest = listtype

    parsing("list<string>") should equal(ListType(StringType))
  }

  they should "parse various primitive types (incomplete list)" in {
    implicit val parserToTest = basetype
    
    parsing("i64") should equal(Int64Type)
    parsing("bool") should equal(BoolType)
  }
}