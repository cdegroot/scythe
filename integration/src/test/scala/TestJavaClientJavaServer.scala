import com.evrl.scythe.integration.Hello
import com.evrl.scythe.integration.Hello.Processor
import java.lang.Thread
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.server.TThreadPoolServer.Args
import org.apache.thrift.server.{TThreadPoolServer, TServer, TSimpleServer}
import org.apache.thrift.transport.{TSocket, TFramedTransport, TServerSocket}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfter, FlatSpec}

/**
 * Benchmark - hook Java client to Java server and make sure they talk
 */
class TestJavaClientJavaServer extends FlatSpec with ShouldMatchers with BeforeAndAfter {
  var server: TServer = null
  
  before {
    val handler = new ScalaServerForGeneratedJavaCode
    val processor = new Hello.Processor(handler)
    val transport = new TServerSocket(9999);
    server = new TThreadPoolServer(new TThreadPoolServer.Args(transport).processor(processor))
    val serverProc = new Runnable { def run = server.serve }
    new Thread(serverProc).start
  }
  after {
    server.stop
  }

  "Java to Java" should "pingpong hi() method" in {
    val transport = new TSocket("localhost", 9999)
    val protocol = new TBinaryProtocol(transport)
    transport.open

    val client = new Hello.Client(protocol)
    client.hi should equal("Hello")
    transport.close
  }
}