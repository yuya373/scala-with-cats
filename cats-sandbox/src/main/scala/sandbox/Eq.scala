import cats.Eq
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._

object CatsEqCat {
  // scala> val cat1 = Cat("Garfield", 38, "orange and black")
  // cat1: Cat = Cat(Garfield,38,orange and black)

  // scala> val cat2 = Cat("Heathcliff", 33, "orange and black")
  // cat2: Cat = Cat(Heathcliff,33,orange and black)

  // scala> val optionCat1 = Option(cat1)
  // optionCat1: Option[Cat] = Some(Cat(Garfield,38,orange and black))

  // scala> val optionCat2 = Option.empty[Cat]
  // optionCat2: Option[Cat] = None

  // scala> import CatsEqCat._
  // import CatsEqCat._

  // scala> cat1 === cat2
  // res26: Boolean = false

  // scala> optionCat1 === optionCat2
  // res28: Boolean = false

  // scala> optionCat2 === optionCat2
  // res29: Boolean = true
  implicit val catEq: Eq[Cat] = Eq.instance((a, b) => {
    a.name === b.name && a.age === b.age && a.color === b.color
  })
}
