import cats.{ Monad => CMonad }
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.Id
import cats.syntax.either._

object Chapter4 {
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).
      flatMap((aNum) => parseInt(bStr).
        flatMap((bNum) => divide(aNum, bNum)))

  trait Monad [F[_]] {
    def pure[A](value: A): F[A]
    def flatmap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatmap(fa)((a: A) => pure(f(a)))
  }
  // Scala cannot unify types and type constructors when searching for implicits
  // Hence our need to re-type Int as Id[Int]
  // sumSquare(3: Id[Int], 4: Id[Int])
  def sumSquare[F[_]: CMonad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  def pure[A](a: A): Id[A] = a
  def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

  def countPositive(nums: List[Int]) =
    // need Either[String, Int]
    // nums.foldLeft(Right(0))((acc, num) => {
    nums.foldLeft(0.asRight[String])((acc, num) => {
      if (num > 0) {
        acc.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    })

  sealed trait LoginError extends Product with Serializable
  final case class UserNotFound(username: String) extends LoginError
  final case class PasswordIncorrect(username: String) extends LoginError
  case object UnexpectedError extends LoginError
  case class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) => println(s"User not found: ${u}")
      case PasswordIncorrect(u) => println(s"Password incorrect: ${u}")
      case UnexpectedError => println(s"Unexpected error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  val result2: LoginResult = UserNotFound("dave").asLeft
  result1.fold(handleError, println)
  result2.fold(handleError, println)
}
