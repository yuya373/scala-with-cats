// there are two functional programming patterns that we shold consider when defining a trait
// - we can make it a typeclass, or
// - we can make it an algebraic data type (and hence seal it)

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.data.NonEmptyList
import cats.implicits._

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

object chapter10_4 {
  sealed trait Predicate[E, A] {
    import Predicate._
    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
    def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) => func(a)
        case And(left, right) =>
          (left(a), right(a)) match {
            case (Invalid(e1), Invalid(e2)) => Invalid(s.combine(e1, e2))
            case (Invalid(e), Valid(_)) => Invalid(e)
            case (Valid(_), Invalid(e)) => Invalid(e)
            case (Valid(_), Valid(_)) => Valid(a)
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

  object Predicate {
    final case class And[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]
    ) extends Predicate[E, A]

    final case class Or[E, A](
      left: Predicate[E, A],
      right: Predicate[E, A]
    ) extends Predicate[E, A]

    final case class Pure[E, A](
      func: A => Validated[E, A]
    ) extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)
    def lift[E, A](e: E, f: A => Boolean): Predicate[E, A] =
      apply((a) => {
        if (f(a)) Valid(a)
        else Invalid(e)
      })
  }

  sealed trait Check[E, A, B] {
    import Check._
    def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E, B]
    def map[C](func: B => C): Check[E, A, C] = Map[E, A, B, C](this, func)
    def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = FlatMap[E, A, B, C](this, func)
    def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen[E, A, B, C](this, that)
  }

  object Check {
    final case class Map[E, A, B, C](
      check: Check[E, A, B],
      func: B => C
    ) extends Check[E, A, C] {
      def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E, C] = check(a).map(func(_))
    }

    final case class FlatMap[E, A, B, C](
      check: Check[E, A, B],
      func: B => Check[E, A, C]
    ) extends Check[E, A, C] {
      def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E, C] = {
        check(a).withEither(_.flatMap(func(_)(a).toEither))

        // import cats.implicits._
        // check(a).toEither.flatMap(func(_)(a).toEither).toValidated

        // check(a) match {
        //   case Invalid(e) => Invalid(e)
        //   case Valid(b) => func(b)(a)
        // }
      }
    }

    final case class AndThen[E, A, B, C](
      check: Check[E, A, B],
      that: Check[E, B, C]
    ) extends Check[E, A, C] {
      def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E, C] = {
        check(a) match {
          case Invalid(e) => Invalid(e)
          case Valid(b) => that(b)
        }
      }
    }

    final case class Pure[E, A](
      pred: Predicate[E, A]
    ) extends Check[E, A, A] {
      def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E, A] = pred(a)
    }

    final case class _Pure[E, A, B](
      f: A => Validated[E, B]
    ) extends Check[E, A, B] {
      def apply(a: A)(implicit s: cats.Semigroup[E]): Validated[E, B] = f(a)
    }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)
    def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] = _Pure(f)
  }

  type Errors = NonEmptyList[String]

  def errors(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      errors(s"Must be longer than ${n} characters"),
      str => str.size > n
    )

  def alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      errors(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      errors(s"Must contain the character ${char}"),
      str => str.contains(char)
    )

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      errors(s"Must contain the character ${char} only once"),
      str => str.filter(c => c == char).size == 1
    )

  def checkUsername: Check[Errors, String, String] =
    Check(longerThan(3).and(alphanumeric))

  def splitEmail: Check[Errors, String, (String, String)] =
    Check((s: String) => s.split('@') match {
      case Array(left, right) => Valid((left, right))
      case _ => Invalid(errors("Must contain @"))
    })

  def checkLeft: Check[Errors, String, String] =
    Check(longerThan(0))

  def checkRight: Check[Errors, String, String] =
    Check(longerThan(3).and(contains('.')))

  def joinEmail: Check[Errors, (String, String), String] =
    Check({ case (left, right) =>
      (checkLeft(left), checkRight(right)).mapN(_ + "@" + _)})

  def checkEmailAddress: Check[Errors, String, String] =
    splitEmail.andThen(joinEmail)

  final case class User(username: String, email: String)

  def createUser(
    username: String,
    email: String
  ): Validated[Errors, User] =
    (checkUsername(username), checkEmailAddress(email)).mapN(User)
}


