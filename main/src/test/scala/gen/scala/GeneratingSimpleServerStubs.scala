package gen.scala

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class GeneratingSimpleServerStubs extends FlatSpec with ShouldMatchers {

  "The simple server stub generator" should "generate basic server code" in {
    val gen = new SimpleServerStubGenerator
    val output = gen.generate("""
      service test {
        string hello(1: string message, 2: i32 echo)
      }""")
    output should equal("""trait test {
  def hello(message: String, echo: Integer): String
}

class testImpl extends test {
  def hello(message: String, echo: Integer): String = {
    // Method body here
    "Hello!"
  }
}

""")
  }
}