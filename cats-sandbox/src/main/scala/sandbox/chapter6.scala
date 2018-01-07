import cats.implicits._
import cats.Semigroupal
import cats.Monoid
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Monad
import cats.data.Validated
import cats.Functor


object chapter6 {
  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt).
      leftMap(ex => s"Couldn't read ${str}")
  // for {
  //   a <- parseInt("a")
  //   b <- parseInt("b")
  //   c <- parseInt("c")
  // } yield (a + b + c)
  // scala> res25: scala.util.Either[String,Int] = Left(Couldn't read a)
  object chapter6_1 {
    // trait Semigroupal[F[_]] {
    //   def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???
    // }
    // Semigroupal[Option].product(Some(123), Some("abc"))

    // scala> Semigroupal[Option].product(Some(123), Some("abc"))
    // res27: Option[(Int, String)] = Some((123,abc))

    // scala> Semigroupal[Option].product(None, Some("abc"))
    // res28: Option[(Nothing, String)] = None

    // scala> Semigroupal[Option].product(Some(123), None)
    // res29: Option[(Int, Nothing)] = None


    // scala> Semigroupal.tuple3(Option(1), Option(2), Option(3))
    // res30: Option[(Int, Int, Int)] = Some((1,2,3))

    // scala> Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
    // res31: Option[(Int, Int, Int)] = None

    // scala> Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
    // res32: Option[Int] = Some(6)

    // scala> Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
    // res33: Option[Int] = None
  }

  object chapter6_2 {

    // scala> (Option(123), Option("abc")).tupled
    // res34: Option[(Int, String)] = Some((123,abc))

    // scala> (Option(123), Option("abc"),Option(true)).tupled
    // res35: Option[(Int, String, Boolean)] = Some((123,abc,true))

    case class Cat(name: String, born: Int, color: String)
    val g = (Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply)
    // res36: Option[chapter6.chapter6_2.Cat] = Some(Cat(Garfield,1978,Orange & black))
    val add: (Int, Int) => Int = (a, b) => a + b
    // (Option(1), Option(2), Option(3)).mapN(add)
    // <console>:101: error: type mismatch;
    // found   : (Int, Int) => Int
    // required: (Int, Int, Int) => ?

    // (Option("cats"), Option(true)).mapN(add)
    // <console>:101: error: type mismatch;
    // found   : (Int, Int) => Int
    // required: (String, Boolean) => ?

  }
  object chapter6_2_1 {
    case class Cat(
      name: String,
      yearOfBirth: Int,
      favoriteFoods: List[String]
    )

    val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply _
    val catToTuple: Cat => (String, Int, List[String]) =
      cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

    implicit val catMonoid: Monoid[Cat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

    val garfield = Cat("Garfield", 1978, List("Lasagne"))
    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
    val combined = garfield |+| heathcliff
    // res40: chapter6.chapter6_2_1.Cat = Cat(GarfieldHeathcliff,3966,List(Lasagne, Junk Food))
  }

  object chapter6_3 {
    val futurePair: Future[(String, Int)] =
      Semigroupal[Future].product(Future("Hello"), Future(123))
    case class Cat(
      name: String,
      yearOfBirth: Int,
      favoriteFoods: List[String]
    )

    val futureCat: Future[Cat] = (
      Future("Garfield"),
      Future(1978),
      Future(List("Lasagne"))
    ).mapN(Cat.apply)

    // scala> Semigroupal[List].product(List(1,2), List(3, 4))
    // res46: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))
    List(1, 2).zip(List(3, 4))

    type ErrorOr[A] = Either[Vector[String], A]
    Semigroupal[ErrorOr].product(Left(Vector("Error 1")), Left(Vector("Error 2")))
    // res1: ErrorOr[(Nothing, Nothing)] = Left(Vector(Error 1))

    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
      x.flatMap(a => y.map(b => (a, b)))
  }
}

object chapter6_4 {
  type AllErrorsOr[A] = Validated[List[String], A]
  val error = Semigroupal[AllErrorsOr].product(Validated.invalid(List("Error 1")), Validated.invalid(List("Error 2")))

  // val v = Validated.Valid(123)
  // res3: cats.data.Validated.Valid[Int] = Valid(123)

  // it is often easier to use the `valid` and `invalid` smart constructors,
  // which siden the return type to Validated
  val v = Validated.valid[List[String], Int](123)
  // v: cats.data.Validated[List[String],Int] = Valid(123)

  // val i = Validated.Invalid(List("Badness"))
  // res4: cats.data.Validated.Invalid[List[String]] = Invalid(List(Badness))
  val i = Validated.invalid[List[String], Int](List("Badness"))
  // i: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

  // we can use `valid` and `invalid` extension methods by importing `cats.syntax.validated`
  // scala> 123.valid[List[String]]
  // res5: cats.data.Validated[List[String],Int] = Valid(123)

  // scala> List("Badness").invalid[Int]
  // res6: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

  type ErrorsOr[A] = Validated[List[String], A]
  // use `pure` in `cats.syntax.applicative._`
  // scala> 123.pure[ErrorsOr]
  // res8: chapter6_4.ErrorsOr[Int] = Valid(123)
  // use `raiseError` from `cats.syntax.applicativeError._`
  // scala> List("Badness").raiseError[ErrorsOr, Int]
  // res11: chapter6_4.ErrorsOr[Int] = Invalid(List(Badness))

  Validated.catchOnly[NumberFormatException]("foo".toInt)
  // res12: cats.data.Validated[NumberFormatException,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")

  Validated.catchNonFatal(sys.error("Badness"))
  // res13: cats.data.Validated[Throwable,Nothing] = Invalid(java.lang.RuntimeException: Badness)

  Validated.fromTry(scala.util.Try("foo".toInt))
  // res14: cats.data.Validated[Throwable,Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")

  Validated.fromEither[String, Int](Left("Badness"))
  // res15: cats.data.Validated[String,Int] = Invalid(Badness)

  Validated.fromOption[String, Int](None, "Badness")
  // res16: cats.data.Validated[String,Int] = Invalid(Badness)
}

object chapter6_4_2 {
  type AllErrorsOr[A] = Validated[String, A]
  // Semigroupal[AllErrorsOr]
  // res17: cats.Semigroupal[chapter6_4_2.AllErrorsOr] = cats.data.ValidatedInstances$$anon$1@8dce4ca

  (
    "Error 1".invalid[Int],
    "Error 2".invalid[Int]
  ).tupled
  // res21: cats.data.Validated[String,(Int, Int)] = Invalid(Error 1Error 2)

  (Vector(404).invalid[Int], Vector(500).invalid[Int]).tupled
  // res22: cats.data.Validated[scala.collection.immutable.Vector[Int],(Int, Int)] = Invalid(Vector(404, 500))


  // scala> 123.valid.map(_ * 100)
  // res23: cats.data.Validated[Nothing,Int] = Valid(12300)

  // scala> "?".invalid.leftMap(_.toString)
  // res24: cats.data.Validated[String,Nothing] = Invalid(?)

  // 123.valid[String].bimap(_ + "!", _ * 100)
  // res25: cats.data.Validated[String,Int] = Valid(12300)

  // scala> "?".invalid[Int].bimap(_ + "!", _ * 100)
  // res26: cats.data.Validated[String,Int] = Invalid(?!)

  // Validated isn't a Monad, we can't flatMap
  // scala> "Badness".invalid[Int].flatMap(a => a)
  // <console>:90: error: value flatMap is not a member of cats.data.Validated[String,Int]
  //        "Badness".invalid[Int].flatMap(a => a)

  // We can convert back and forth between Validated and Either using `toEither` and `toValidated` methods
  // scala> "Badness".invalid[Int].toEither
  // res31: Either[String,Int] = Left(Badness)
  // scala> "Badness".invalid[Int].toEither.toValidated
  // res34: cats.data.Validated[String,Int] = Invalid(Badness)

  // We can use `withEither` method to temporarily convert to an Either and convert back again immediately
  // scala> 41.valid[String].withEither(_.flatMap(n => Right(n + 1)))
  // res35: cats.data.Validated[String,Int] = Valid(42)

  // scala> "fail".invalid[Int].getOrElse(0)
  // res38: Int = 0

  // scala> "fail".invalid[Int].fold(_ + "!!!", _.toString)
  // res39: String = fail!!!
}

object chapter6_4_4 {
  type Name = String
  type Age = Int
  case class User(name: Name, age: Age)
  type ErrorsOrUser = Validated[List[String], User]
  type Input = Map[String, String]

  def getValue(input: Input, key: String): Either[List[String], String] = {
    input.get(key).toRight(List(s"${key} field not specified"))
  }

  def nonBlank(name: String)(str: String): Either[List[String], String] = {
    Right(str).ensure(List(s"${name} must not blank"))(_.nonEmpty)
  }

  def readName(input: Input): Either[List[String], Name] = {
    getValue(input, "name").flatMap(nonBlank("name")(_))
  }

  def parseInt(name: String)(input: String): Either[List[String], Int] = {
    Either.catchOnly[NumberFormatException](input.toInt).
      leftMap(_ => List(s"${name} must be integer"))
  }

  def nonNegative(name: String)(int: Int): Either[List[String], Int] = {
    Right(int).ensure(List(s"${name} must not negative"))(_ >= 0)
  }

  def readAge(input: Input): Either[List[String], Age] = {
    getValue(input, "age").flatMap(ageStr => {
      parseInt("age")(ageStr).flatMap(nonNegative("age"))
    })
  }

  def readUser(input: Input): Validated[List[String], User] = {
    (
      readName(input).toValidated,
      readAge(input).toValidated
    ).mapN(User.apply)
  }
}

object chapter6_5 {
  trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
    // applies `fa` to `ff` within a context F[_]
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val ff: F[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(ff)(fb)
    }
  }

  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](a: A): F[A]
  }
}
