import cats.data.OptionT
import cats.Monad
// import cats.instances.list._
// import cats.syntax.applicative._
import cats.implicits._

object chapter5 {
  case class User(name: String)
  def lookupUser(id: Long): Either[Error, Option[User]] = ???
  def lookupUserName(id: Long): Either[Error, Option[String]] = {
    for {
      optUser <- lookupUser(id)
    } yield {
      for {
        user <- optUser
      } yield user.name
    }
  }
}

object chapter5_2 {
  // to transform a List[Option[A]] into a single monad
  // we pass List, the type of the outer monad as a parameter to OptionT
  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]
  // result1.flatMap(x => result2.map(y => x + y))
}
