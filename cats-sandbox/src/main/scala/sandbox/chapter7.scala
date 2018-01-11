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

}
