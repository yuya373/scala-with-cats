import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.Id
import cats.Applicative
import cats.syntax.functor._

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
    // assert(actual == expected)
    // <pastie>:38: warning: scala.concurrent.Future[Int] and Int are unrelated: they will most likely never compare equal
    //     assert(actual == expected)
  }
}

object chapter81 {
  // trait RealUptimeClient extends UptimeClient {
  //   def getUptime(hostname: String): Future[Int]
  // }

  // trait TestUptimeClient extends UptimeClient {
  //   def getUptime(hostname: String): Int
  // }

  // trait UptimeClient {
  //   // We need to abstract over Future[Int] and Int
  //   def getUptime(hostname: String): ???
  // }

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int] =
      hosts.getOrElse(hostname, 0)
  }


  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      // need `import cats.syntax.functor._` for F[List[Int]].map
      // This is because we're switching from using future.map to the Cats' generic extension method that requires an implicit Functor parameter
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual: Int = service.getTotalUptime(hosts.keys.toList)
    val expected: Int = hosts.values.sum
    assert(actual == expected)
  }
}
