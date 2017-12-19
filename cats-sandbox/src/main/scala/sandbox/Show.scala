import cats.Show

object CatsShowCat {
  // scala> val cat = Cat("tora", 8, "yellow")
  // cat: Cat = Cat(tora,8,yellow)

  // scala> import CatsShowCat._
  // import CatsShowCat._

  // scala> import cats.syntax.show._
  // import cats.syntax.show._

  // scala>  cat.show
  implicit val catShow: Show[Cat] =
    Show.show((cat) => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
}
