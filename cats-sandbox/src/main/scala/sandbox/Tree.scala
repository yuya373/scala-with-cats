import cats.Functor
import cats.Monad
import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
    }
  }

  implicit val treeMonad = new Monad[Tree] {
    def pure[A](a: A): Tree[A] = leaf(a)
    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      fa match {
        case Leaf(value) => f(value)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }
    }
    // https://stackoverflow.com/questions/44504790/cats-non-tail-recursive-tailrecm-method-for-monads
    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(l: List[Tree[Either[A, B]]], acc: List[Tree[B]]): List[Tree[B]] = {
        l match {
          case Nil => acc
          case Leaf(Left(a)) :: tail => loop(f(a) :: tail, acc)
          case Leaf(Right(b)) :: tail => loop(
            tail,
            acc match {
              case Nil => pure(b) :: acc
              case head :: tail => Branch(head, pure(b)) :: tail
            }
          )
          case Branch(l, r) :: tail => l match {
            case Leaf(Left(a)) => loop(f(a) :: r :: tail, acc)
            case Leaf(Right(b)) => loop(r :: tail, pure(b) :: acc)
            case Branch(_, _) => loop(l :: r :: tail, acc)
          }
        }
      }

      loop(List(f(a)), Nil).head
    }
    // def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
    //   f(a) match {
    //     case Leaf(Left(a)) => tailRecM(a)(f)
    //     case Leaf(Right(b)) => Leaf(b)
    //     case Branch(l, r) => Branch(
    //       flatMap(l) {
    //         case Left(l) => tailRecM(l)(f)
    //         case Right(l) => pure(l)
    //       },
    //       flatMap(r) {
    //         case Left(r) => tailRecM(r)(f)
    //         case Right(r) => pure(r)
    //       }
    //     )
    //   }
    // }
  }
}
