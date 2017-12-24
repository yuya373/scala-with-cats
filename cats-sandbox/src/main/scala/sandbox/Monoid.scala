trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid

  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = x && y
    def empty: Boolean = true
  }
  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = x || y
    def empty: Boolean = false
  }
  implicit val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = (x && !y) || (!x && y)
    def empty: Boolean = false
  }
  implicit val booleanXnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(x: Boolean, y: Boolean) = (!x || y) && (x || !y)
    def empty: Boolean = true
  }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int) = x + y
  }
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty[A]
    // https://docs.scala-lang.org/overviews/collections/sets.html
    // ++ inherits from Traversable
    // def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
    def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  // scala> val intSetMonoid = Monoid[Set[Int]]
  // intSetMonoid: Monoid[Set[Int]] = Monoid$$anon$6@57bd0b3c

  // scala> intSetMonoid.combine(Set(1, 2), Set(2, 99))
  // res0: Set[Int] = Set(1, 2, 99)
  implicit def setIntersectionSemigroup[A] = new Semigroup[Set[A]] {
    // No identity element for set intersection
    def combine(x: Set[A], y: Set[A]) = x & y
  }
  implicit def setDiffMonoid[A] = new Monoid[Set[A]] {
    def combine(x: Set[A], y: Set[A]) = x diff y
    def empty: Set[A] = Set.empty
  }
}

object MonoidLaws {
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    // 1 + (2 + 3) == (1 + 2) + 3
    // (1 - 2) - 3 = -4
    // 1 - (2 - 3) = 2
    // Integer subtraction is not a monoid
    m.combine(x, m.combine(y, z)) ==
    m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    // (2 + 0) == 2 && (0 + 2) == 2
    (m.combine(x, m.empty) == x) &&
    (m.combine(m.empty, x) == x)
  }
}
