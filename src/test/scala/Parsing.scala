
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

  they should "parse function declarations" in {
    implicit val parserToTest = function

    parsing("void hello()") should equal(Function(VoidType, Identifier("hello"), List(), List(), false))
    parsing("oneway void hello()") should equal(Function(VoidType, Identifier("hello"), List(), List(), true))
    parsing("i32 buzzer(1: i32 length, 2: bool loud)") should equal(Function(Int32Type, Identifier("buzzer"),
      List(Field(Int32Type, Identifier("length"), Some(IntegerConstant(1)), false),
           Field(BoolType, Identifier("loud"), Some(IntegerConstant(2)), false)), List(), false))

    parsing("void hello() throws (1: FooException foo)") should equal(Function(VoidType, Identifier("hello"), List(),
      List(Field(ComplexType(Identifier("FooException")), Identifier("foo"), Some(IntegerConstant(1)), false)), false))
  }
}