import cats.{ Monad => CMonad }
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.Id

object Chapter4 {
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).
      flatMap((aNum) => parseInt(bStr).
        flatMap((bNum) => divide(aNum, bNum)))

  trait Monad [F[_]] {
    def pure[A](value: A): F[A]
    def flatmap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatmap(fa)((a: A) => pure(f(a)))
  }
  // Scala cannot unify types and type constructors when searching for implicits
  // Hence our need to re-type Int as Id[Int]
  // sumSquare(3: Id[Int], 4: Id[Int])
  def sumSquare[F[_]: CMonad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  def pure[A](a: A): Id[A] = a
  def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
}
