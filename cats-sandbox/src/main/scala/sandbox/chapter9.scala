import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._

object chapter9 {
  def foldMap[A, B: cats.Monoid](l: Vector[A])(f: A => B)(implicit m: cats.Monoid[B]): B =
    l.map(f).fold(m.empty)(m.combine)

  // scala> import cats.implicits._
  // import cats.implicits._

  // scala> chapter9.foldMap(Vector(1, 2, 3))(a => a * 2)
  // res3: Int = 12

  // scala> chapter9.foldMap(Vector(1, 2, 3))(identity)
  // res4: Int = 6

  // scala> chapter9.foldMap(Vector(1, 2, 3))(_.toString + "! ")
  // res5: String = "1! 2! 3! "

  // scala> chapter9.foldMap("Hello World!".toVector)(_.toString.toUpperCase)
  // res6: String = HELLO WORLD!


  val numberOfCPUs = Runtime.getRuntime.availableProcessors()
  // scala> chapter9.numberOfCPUs
  // res7: Int = 4
  // scala> (1 to 10).toList.grouped(4).toList
  // res9: List[List[Int]] = List(List(1, 2, 3, 4), List(5, 6, 7, 8), List(9, 10))

  def parallelFoldMap[A, B: cats.Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val n = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.size / n).ceil.toInt
    // val fl = values.grouped(groupSize).map(v => Future {
    //   v.foldMap(func)
    //   // foldMap(v)(func)
    // }).toVector
    // // Future.sequence(fl).map(l => foldMap(l)(identity(_)))
    // fl.sequence.map(vec => vec.foldMap(identity(_)))

    val fl: Future[Vector[B]] = values.grouped(groupSize).toVector.
      traverse(vec => Future(vec.foldMap(func)))
    fl.map(vec => vec.combineAll)
  }
}
