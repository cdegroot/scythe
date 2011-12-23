
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
  
  they should "parse a set definition" in {
    implicit val parserToTest = settype
    
    parsing("set<double>") should equal(SetType(DoubleType))
  }
  
  they should "parse a map definition" in {
    implicit val parserToTest = maptype
    
    parsing("map<i32,string>") should equal(MapType(Int32Type, StringType))
  }
  
  they should "parse various field definitions" in {
    implicit val parserToTest = field
    
    parsing("i32 foo") should equal(Field(Int32Type, Identifier("foo"), None, false))
    parsing("1: bool bar") should equal(Field(BoolType, Identifier("bar"), Some(IntegerConstant(1)), false))
    parsing("2: required string quux") should equal(Field(StringType, Identifier("quux"), Some(IntegerConstant(2)), true))
  }
}