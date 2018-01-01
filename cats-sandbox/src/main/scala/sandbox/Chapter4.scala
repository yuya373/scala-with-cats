import cats.{ Monad => CMonad }
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.Id
import cats.syntax.either._
import cats.{ MonadError => CMonadError }
import cats.instances.either._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import scala.util.Try
import cats.instances.try_._
import cats.Eval

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
  // result1.fold(handleError, println)
  // result2.fold(handleError, println)

  trait MonadError[F[_], E] extends Monad[F] {
    // Lift an error into the `F` context
    def raiseError[A](e: E): F[A]
    // Handle an error, potentially recovering from it:
    def handleError[A](fa: F[A])(f: E => A): F[A]
    // Test an instance of `F`, fail if the predicate is not satisfied
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  type ErrorOr[A] = Either[String, A]
  val monadError = CMonadError[ErrorOr, String]
  // val success = monadError.pure(42)
  val success = 42.pure[ErrorOr]
  // val failure = monadError.raiseError("Badness")
  val failure = "Badness".raiseError[ErrorOr, Int]
  // monadError.handleError(failure)(str => str match {
  //   case "Badness" => monadError.pure("It's ok")
  //   case other => monadError.raiseError("It's not ok")
  // })
  // monadError.ensure(success)("Number too low!")(_ > 1000)
  success.ensure("Number to low!")(_ > 1000)

  val exn: Throwable = new RuntimeException("It's all gone wrong")
  exn.raiseError[Try, Int]

  val x = {
    println("Computing X")
    math.random()
  }

  def y = {
    println("Computing Y")
    math.random()
  }

  lazy val z = {
    println("Computing Z")
    math.random()
  }

  val now = Eval.now(math.random() + 1000)
  val later = Eval.later(math.random() + 2000)
  val always = Eval.always(math.random() + 3000)

  val _x = Eval.now({
    println("Computing X")
    math.random()
  })

  val _y = Eval.always {
    println("Computing Y")
    math.random()
  }

  val _z = Eval.later {
    println("Computing Z")
    math.random()
  }

  val greeting = Eval.always {
    println("Step 1")
    "Hello"
  }.map { str =>
    println("Step 2")
    s"${str} world"
  }

  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 }
    b <- Eval.always { println("Calculating B"); 2 }
  } yield {
    println("Adding A and B")
    a + b
  }

  val saying = Eval.
    always { println("Step 1"); "The cat" }.
    map { str => println("Step 2"); s"${str} sat on" }.
    memoize.
    map { str => println("Step 3"); s"${str} the mat" }

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(
        factorial(n - 1).map(_ * n)
      )
    }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      // Stack Over flow
      // case head :: tail => fn(head, foldRightEval(tail, acc)(fn))
      case Nil => acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc))((a, b) => b.map(fn(a, _))).value
}
