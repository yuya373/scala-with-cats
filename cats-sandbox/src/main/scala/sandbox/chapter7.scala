import cats.Foldable
import cats.instances.list._
import cats.instances.option._
import cats.Eval
import cats.instances.stream._
import cats.instances.int._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Applicative
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.instances.vector._
import cats.syntax.traverse._

object chapter711 {
  def show[A](list: List[A]): String =
    list.foldLeft("nil")((acc, e) => s"${e} then ${acc}")

  // scala> show(Nil)
  // res57: String = nil

  // scala> show(List(1, 2, 3))
  // res58: String = 3 then 2 then 1 then nil

}

object chapter712 {

  // scala> List(1, 2, 3).foldLeft(Nil: List[Int])((acc, e) => e :: acc)
  // res62: List[Int] = List(3, 2, 1)

  // scala> List(1, 2, 3).foldRight(Nil: List[Int])((e, acc) => e :: acc)
  // res64: List[Int] = List(1, 2, 3)

}

object chapter713 {
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l.foldLeft(List.empty[B])((acc, e) => f(e) :: acc).reverse
  }
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    l.foldLeft(List.empty[B])((acc, e) => f(e) ++ acc).reverse
  }
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    l.foldLeft(List.empty[A])((acc, e) => {
      if (f(e)) e :: acc
      else acc
    }).reverse
  }
  def sum[A](l: List[A])(implicit m: Monoid[A]): A = {
    l.foldLeft(m.empty)((acc, e) => m.combine(acc, e))
  }
  case class User(age: Int)
  implicit val userMonoid = new Monoid[User] {
    def empty: User = User(0)
    def combine(x: User, y: User): User = User(x.age + y.age)
  }
  val totalAge = sum(List(User(20), User(40)))
}

object chapter714 {
  val ints = List(1, 2, 3)

  Foldable[List].foldLeft(ints, 0)(_ + _)
  // res77: Int = 6

  val maybeInt = Option(123)
  Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

  def bigData = (1 to 100000).toStream
  // scala> chapter714.bigData.foldRight(0L)(_ + _)
  // java.lang.StackOverflowError
  val foldableForStream: Foldable[Stream] = Foldable[Stream]
  val eval: Eval[Long] = foldableForStream.
    foldRight(bigData, Eval.now(0L)) { (num, eval) => eval.map(_ + num)}


  // scala> Foldable[Option].nonEmpty(None)
  // res7: Boolean = false

  // scala> Foldable[Option].nonEmpty(Option(42))
  // res8: Boolean = true

  // scala> Foldable[List].find(List(1, 2, 3))(a => (a % 2) == 0)
  // res10: Option[Int] = Some(2)

  // Foldable[List].combineAll(List(1, 2, 3))
  // res12: Int = 6

  // Foldable[List].foldMap(List(1, 2, 3))(_.toString)
  // res13: String = 123

  // We can compose Foldables to support deep traversal of nested sequences
  val _ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val composed = (Foldable[List] compose Foldable[Vector]).combineAll(_ints)


  // scala> List(1, 2, 3).combineAll
  // res15: Int = 6

  // scala> List(1, 2, 3).foldMap(_.toString)
  // res16: String = 123
}

object chapter721 {
  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length() * 60)

  // val allUptimes: Future[List[Int]] =
  //   hostnames.foldLeft(Future(List.empty[Int])) { (acc, host) =>
  //     val uptime = getUptime(host)
  //     for {
  //       acc <- acc
  //       uptime <- uptime
  //     } yield acc :+ uptime
  //   }
  val allUptimes: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)

  // scala> Await.result(allTimes, 1.second)
  // res0: List[Int] = List(1020, 960, 840)
}

object chapter722 {
  // scala> List.empty[Int].pure[Future]
  // res4: scala.concurrent.Future[List[Int]] = Future(Success(List()))

  def oldCombine(
    acc: Future[List[Int]],
    host: String
  ): Future[List[Int]] = {
    val uptime = chapter721.getUptime(host)
    for {
      acc <- acc
      uptime <- uptime
    } yield acc :+ uptime
  }

  def newCombine(
    acc: Future[List[Int]],
    host: String
  ): Future[List[Int]] =
    (acc, chapter721.getUptime(host)).mapN((l, i) => l :+ i)

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F])((acc: F[List[B]], e: A) => {
      val f = (l: List[B], a: B) => l :+ a
        (acc: F[List[B]], func(e): F[B]).mapN(f)
    })

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity(_))

  val totalUptime: Future[List[Int]] =
    listTraverse(chapter721.hostnames)(chapter721.getUptime)

  val l: List[Vector[Int]] = List(Vector(1, 2), Vector(3, 4))
  val v: Vector[List[Int]] = listSequence(l)
  // scala> chapter722.v
  // res0: Vector[List[Int]] = Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

  val vl = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  // scala> chapter722.vl
  // res2: scala.collection.immutable.Vector[List[Int]] = Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)
  // scala> chapter722.process(List(2, 4, 6))
  // res3: Option[List[Int]] = Some(List(2, 4, 6))

  // scala> chapter722.process(List(1, 2, 3))
  // res4: Option[List[Int]] = None
}

object chapter723 {
  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative, A, B](inputs: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Applicative, A, B](inputs: F[G[B]]): G[F[B]] =
      traverse(inputs)(identity(_))
  }

  val numbers: List[Future[Int]] = List(Future(1), Future(2), Future(3))

  val numbers2: Future[List[Int]] = cats.Traverse[List].sequence(numbers)
  // scala> Await.result(chapter723.numbers2, 1.second)
  // res0: List[Int] = List(1, 2, 3)

  // scala> Await.result(chapter723.numbers.sequence, 1.second)
  // res6: List[Int] = List(1, 2, 3)
}
