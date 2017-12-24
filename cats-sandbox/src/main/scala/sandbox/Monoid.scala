trait Monoid[A] {
  def combine(x: A, y: A): A
  def empty: A
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
