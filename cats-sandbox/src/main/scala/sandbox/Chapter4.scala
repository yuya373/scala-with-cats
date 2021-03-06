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
import cats.data.Writer
import cats.instances.vector._
import cats.syntax.writer._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.data.Reader
import cats.data.State
import scala.annotation.tailrec

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

  type Logged[A] = Writer[Vector[String], A]

  // 123.pure[Logged]
  // Vector("msg1", "msg2", "msg3").tell
  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))

  val aResult: Int = a.value
  val aLog: Vector[String] = a.written

  val (log, result) = b.run
  // it's good practice to use a log type
  // that has an efficient append and concatenate opreations,
  // such as a Vector
  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  // val writer1 = 10.pure[Logged].flatMap((a) => {
  //   Vector("a", "b", "c").tell.flatMap(_ => {
  //     32.writer(Vector("x", "y", "z")).map((b) => a + b)
  //   })
  // })

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase()),
    res => res * 100
  )
  val writer4 = writer1.mapBoth { (log, res) => (log.map(_ + "!"), res * 1000) }
  val writer5 = writer1.reset
  val writer6 = writer1.swap

  def slowly[A](body: => A): A =
    try body finally Thread.sleep(1000)
  def factorial(n: Int): Logged[Int] = {
    if (n == 0) 1.pure[Logged]
    else slowly(factorial((n - 1): Int).flatMap((num) => {
      val ans = num * n
      ans.writer(Vector(s"fact: ${n} -> ${ans}"))
    }))
    // for {
    //   ans <- if (n == 0) 1.pure[Logged]
    //   else slowly(factorial((n - 1): Int).map(_ * n))
    //   _ <- Vector(s"fact: ${n} -> ${ans}").tell
    // } yield ans
  }

  def exercise473 = {
    Await.result(
      Future.sequence(
        Vector(
          Future(factorial(3: Int)),
          Future(factorial(5: Int))
        )
      ),
      5000.seconds
    )
  }

  case class Cat(name: String, favoriteFood: String)
  val catName: Reader[Cat, String] = Reader(cat => cat.name)
  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello ${name}")

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
  val greetAndFeed: Reader[Cat, String] =
    greetKitty.flatMap { greet =>
      feedKitty.map { feed =>
        s"${greet}. ${feed}"
      }
    }

  type Username = String
  type Password = String
  type UserId = Int
  case class Db(usernames: Map[UserId, Username], passwords: Map[Username, Password])
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: UserId): DbReader[Option[Username]] =
    Reader((db: Db) => db.usernames.get(userId))

  def checkPassword(username: Username, password: Password): DbReader[Boolean] =
    Reader((db: Db) => db.passwords.get(username).contains(password))

  def checkLogin(userId: UserId, password: Password): DbReader[Boolean] =
    findUsername(userId).flatMap {
      _ match {
        case Some(username) => checkPassword(username, password)
        case _ => false.pure[DbReader]
      }
    }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  // val state = State[Int, String] { state =>
  //   (state, s"The state is ${state}")
  // }

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: ${ans}")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: ${ans}")
  }

  val both = step1.flatMap { a => step2.map(b => (a, b)) }
  val (state, _result) = both.run(20).value

  val program: State[Int, (Int, Int, Int)] = {
    import State._
    get[Int].flatMap { a =>
      set[Int](a + 1).flatMap { _ =>
        get[Int].flatMap { b =>
          modify[Int](_ + 1).flatMap { _ =>
            inspect[Int, Int](_ * 1000).map { c =>
              (a, b, c)
            }
          }
        }
      }
    }
  }

  type CalcState[A] = State[List[Int], A]
  // def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
  //   val newStack =
  //     if (sym == "+") {
  //       val op1 :: op2 :: tail = oldStack
  //       (op1 + op2) :: tail
  //     } else if (sym == "*") {
  //       val op1 :: op2 :: tail = oldStack
  //       (op1 * op2) :: tail
  //     } else {
  //       sym.toInt :: oldStack
  //     }
  //   val result :: _ = newStack

  //   (newStack, result)
  // }
  def operator(f: (Int, Int) => Int): CalcState[Int] = {
    State[List[Int], Int] { oldStack =>
      oldStack match {
        case a :: b :: tail => {
          val ans = f(a, b)
          (ans :: tail, ans)
        }
        case _ => sys.error("Fail")
      }
    }
  }
  def operand(num: Int): CalcState[Int] = State[List[Int], Int] { oldStack =>
    (num :: oldStack, num)
  }
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator((a, b) => a + b)
    case "-" => operator((a, b) => a - b)
    case "*" => operator((a, b) => a * b)
    case "/" => operator((a, b) => a / b)
    case num => operand(num.toInt)
  }
  def evalAll(input: List[String]): CalcState[Int] = {
    // val head :: tail = input
    // tail.foldLeft(evalOne(head)) { (s, sym) => s.flatMap(_ => evalOne(sym)) }
    input.foldLeft(0.pure[CalcState]) { (a, b) => a.flatMap(_ => evalOne(b)) }
  }
  // val prog = evalAll(List("1", "2", "+", "3", "*"))
  // val prog = evalAll(List("1", "2", "+")).flatMap { _ => evalAll(List("3", "4", "+")).flatMap { _ => evalOne("*") } }
  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  val optionMonad = new CMonad[Option] {
    def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] =
      opt.flatMap(fn)

    def pure[A](opt: A): Option[A] = Some(opt)

    // http://functorial.com/stack-safety-for-free/index.pdf
    @tailrec
    def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
  }
}
