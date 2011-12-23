
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
    
    parsing("i32 foo") should equal(Field(Int32Type, "foo", None, false))
    parsing("1: bool bar") should equal(Field(BoolType, "bar", Some(IntegerConstant(1)), false))
    parsing("2: required string quux") should equal(Field(StringType, "quux", Some(IntegerConstant(2)), true))
  }

  def buzzerFunctionAst: Function = {
    Function(Int32Type, "buzzer",
      List(Field(Int32Type, "length", Some(IntegerConstant(1)), false),
        Field(BoolType, "loud", Some(IntegerConstant(2)), false)), List(), false)
  }

  def helloFunctionAst: Function = {
    Function(VoidType, "hello", List(), List(), false)
  }

  they should "parse function declarations" in {
    implicit val parserToTest = function

    parsing("void hello()") should equal(helloFunctionAst)
    parsing("oneway void hello()") should equal(Function(VoidType, "hello", List(), List(), true))
    parsing("i32 buzzer(1: i32 length, 2: bool loud)") should equal(buzzerFunctionAst)

    parsing("void hello() throws (1: FooException foo)") should equal(Function(VoidType, "hello", List(),
      List(Field(ComplexType("FooException"), "foo", Some(IntegerConstant(1)), false)), false))
  }

  they should "parse service definitions" in {
    implicit val parserToTest = service

    parsing("""
      service test {
        void hello(),
        i32 buzzer(1: i32 length, 2: bool loud)
      }""") should equal(Service("test", None, List(helloFunctionAst, buzzerFunctionAst)))

    parsing("""
      service test extends base {
        void hello(),
        i32 buzzer(1: i32 length, 2: bool loud)
      }""") should equal(Service("test", Some("base"), List(helloFunctionAst, buzzerFunctionAst)))
  }

  they should "parse exception definitions" in {
    implicit val parserToTest = exception

    parsing("exception foo {}") should equal(Exception("foo", List()));
    parsing("exception foo {1: i32 why, 2: bool fatal}") should equal(Exception("foo",
      List(Field(Int32Type, "why", Some(IntegerConstant(1)), false),
           Field(BoolType, "fatal", Some(IntegerConstant(2)), false))))
  }
}