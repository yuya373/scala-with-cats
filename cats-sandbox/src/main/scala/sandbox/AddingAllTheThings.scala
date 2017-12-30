// the type class
import cats.{Monoid => CMonoid}
// instances
import cats.instances.double._
// syntax
import cats.syntax.semigroup._

object AddingAllTheThings {
  // def add(items: List[Int])(implicit ev: CMonoid[Int]): Int = {
  //   items.foldLeft(CMonoid[Int].empty)(_ |+| _)
  // }
  def add[A](items: List[A])(implicit ev: CMonoid[A]): A = {
    items.foldLeft(CMonoid[A].empty)(_ |+| _)
  }

  // scala> import AddingAllTheThings._
  // import AddingAllTheThings._

  // scala> add(List(1, 2, 3))
  // res10: Int = 6

  // scala> add(List(Some(1), None, Some(2), None, Some(3)))
  // res11: Option[Int] = Some(6)

  // scala> add(List(Some(1), Some(2), Some(3)))
  // <console>:69: error: could not find implicit value for parameter ev: cats.Monoid[Some[Int]]
  //        add(List(Some(1), Some(2), Some(3)))
  // scala> add(List(Some(1), Some(2), Some(3)): List[Option[Int]])
  // res13: Option[Int] = Some(6)

  case class Order(totalCost: Double, quantity: Double)
  implicit def orderMonoid = new CMonoid[Order] {
    def empty: Order = Order(0.0, 0.0)
    def combine(x: Order, y: Order) = {
      Order(
        x.totalCost |+| y.totalCost,
        x.quantity |+| y.quantity
      )
    }
  }
}
