import com.evrl.scythe.integration.Hello

/**
 * This class interacts with the generated java code and thus acts as a bench-mark
 */
class ScalaServerForGeneratedJavaCode extends Hello.Iface {
  def hi() = "Hello"
}