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
