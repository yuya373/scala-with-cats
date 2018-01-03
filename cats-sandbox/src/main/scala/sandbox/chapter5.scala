import cats.data.{ EitherT, OptionT, Writer }
// import cats.instances.list._
// import cats.syntax.applicative._
import cats.implicits._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object chapter5 {
  case class User(name: String)
  def lookupUser(id: Long): Either[Error, Option[User]] = ???
  def lookupUserName(id: Long): Either[Error, Option[String]] = {
    for {
      optUser <- lookupUser(id)
    } yield {
      for {
        user <- optUser
      } yield user.name
    }
  }
}

object chapter5_2 {
  // to transform a List[Option[A]] into a single monad
  // we pass List, the type of the outer monad as a parameter to OptionT
  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]
  // result1.flatMap(x => result2.map(y => x + y))
}

object chapter5_3 {
  // Either itself has two type parameters and monads only have one
  // We need a type alias to convert the type constructor to the correct shape
  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val futureEitherOr: FutureEitherOption[Int] = 10.pure[FutureEitherOption].flatMap(a => 32.pure[FutureEitherOption].map(b => a + b))
  // scala> chapter5_3.futureEitherOr.value
  // res12: chapter5_3.FutureEither[Option[Int]] = EitherT(Future(Success(Right(Some(42)))))

  // scala> chapter5_3.futureEitherOr.value.value
  // res13: scala.concurrent.Future[Either[String,Option[Int]]] = Future(Success(Right(Some(42))))

  // scala> Await.result(chapter5_3.futureEitherOr.value.value, 5.seconds)
  // res14: Either[String,Option[Int]] = Right(Some(42))

  object chapter5_3_5 {
    // Usage Patterns
    // expose untransformed stacks at module boundaries,
    // transform them to operate on them locally,
    // and untransform them before passing them on
    // This allows each module of code to make its own decisions about which transformers to use
    type Logged[A] = Writer[List[String], A]
    // Methods generally return untransformed stacks
    def parseNumber(str: String): Logged[Option[Int]] =
      util.Try(str.toInt).toOption match {
        case Some(num) => Writer(List(s"Read ${str}"), Some(num))
        case None => Writer(List(s"Failed on ${str}"), None)
      }
    // Consumers use monad transformers locally to simplify composition
    def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
      import cats.data.OptionT
      val result = OptionT(parseNumber(a)).flatMap { a =>
        OptionT(parseNumber(b)).flatMap { b =>
          OptionT(parseNumber(c)).map(c => a + b + c)
        }
      }

      result.value
    }
  }
  import chapter5_3_5._
  // This approach doesn't force OptionT on other user's code
  val result1 = addAll("1", "2", "3")
  val result2 = addAll("1", "a", "3")

  object chapter5_4 {
    import cats.data.EitherT
    // type Response[A] = Future[Either[String, A]]
    type Response[A] = EitherT[Future, String, A]
    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )

    def getPowerLevel(autobot: String): Response[Int] = {
      // Future(powerLevels.get(autobot).toRight(s"Not found: ${autobot}"))
      EitherT(
        Future(
          powerLevels.get(autobot).toRight(s"Not found: ${autobot}")
        )
      )
    }

    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
      for {
        p1 <- getPowerLevel(ally1)
        p2 <- getPowerLevel(ally2)
      } yield (p1 + p2) > 15
    }

    def tacticalReport(ally1: String, ally2: String): String = {
      val s = canSpecialMove(ally1, ally2).
        map { canMove =>
          if (canMove) s"${ally1} and ${ally2} are ready to roll out!"
          else s"${ally1} and ${ally2} need a recharge"
        }
      Await.result(s.value, 5.second) match {
        case Left(error) => s"Comms error: ${error}"
        case Right(msg) => msg
      }
    }
  }
}
