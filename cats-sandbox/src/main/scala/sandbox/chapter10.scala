// there are two functional programming patterns that we shold consider when defining a trait
// - we can make it a typeclass, or
// - we can make it an algebraic data type (and hence seal it)

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

object chapter10 {
  // type Check[E, A] = A => Either[E, A]
  sealed trait Check[E, A] {
    // We don't need a full Monoid because we don't need the `identity`.
    // We should always try to keep our constrains as small as possible!
    def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E, A] = {
      this match {
        case Pure(func) => func(a)
        case And(left, right) =>
          (left(a), right(a)) match {
            case (Invalid(e1), Invalid(e2)) => Invalid(s.combine(e1, e2))
            case (Invalid(e), Valid(a)) => Invalid(e)
            case (Valid(a), Invalid(e)) => Invalid(e)
            case (Valid(a1), Valid(a2)) => Valid(a)
          }
        case Or(left, right) =>
          left(a) match {
            case Valid(_) => Valid(a)
            case Invalid(e1) => right(a) match {
              case Valid(_) => Valid(a)
              case Invalid(e2) => Invalid(s.combine(e1, e2))
            }
          }
      }
    }
    def and(that: Check[E, A]): Check[E, A] = And(this, that)
    def or(that: Check[E, A]): Check[E, A] = Or(this, that)
  }

  final case class And[E, A](
    left: Check[E, A],
    right: Check[E, A]
  ) extends Check[E, A]

  final case class Or[E, A](
    left: Check[E, A],
    right: Check[E, A]
  ) extends Check[E, A]

  final case class Pure[E, A](
    func: A => Validated[E, A]
  ) extends Check[E, A]

  val a = Pure((v: Int) => {
    if (v > 2) Valid(v)
    else Invalid(List("Must be > 2"))
  })

  val b = Pure((v: Int) => {
    if (v < -2) Valid(v)
    else Invalid(List("Must be < -2"))
  })

  val check = a and b

  val checkOr = a or b

  // ADT implementation is more verbose than the function wrapper implementation,
  // it has the advantage of cleanly separating the structure of the computation (the ADT instance we create)
  // from process that gives it meaning (the apply method)

  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = func(a)

    def and(that: CheckF[E, A])(implicit s: cats.Semigroup[E]): CheckF[E, A] =
      CheckF((a: A) => {
        (this(a), that(a)) match {
          case (Left(e1), Left(e2)) => Left(s.combine(e1, e2))
          case (Left(e), Right(a)) => Left(e)
          case (Right(a), Left(e)) => Left(e)
          case (Right(a1), Right(a2)) => Right(a)
        }
      })
  }

}

