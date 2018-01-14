import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._

trait UptimeClient {
  def getUptime(hostname: String): Future[Int]
}

class UptimeService(client: UptimeClient) {
  def getTotalUptime(hostnames: List[String]): Future[Int] =
    hostnames.traverse(client.getUptime(_)).map(_.sum)
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient {
  def getUptime(hostname: String): Future[Int] =
    Future.successful(hosts.getOrElse(hostname, 0))
}

object chapter8 {
  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual: Future[Int] = service.getTotalUptime(hosts.keys.toList)
    val expected: Int = hosts.values.sum
    assert(actual == expected)
    // <pastie>:38: warning: scala.concurrent.Future[Int] and Int are unrelated: they will most likely never compare equal
    //     assert(actual == expected)
  }
}
