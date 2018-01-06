import cats.implicits._
import cats.Semigroupal
import cats.Monoid
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Monad


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
