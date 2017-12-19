import java.util.Date
import cats.Eq
import cats.syntax.eq._
import cats.instances.long._

object DateEq {
  implicit val dateEq: Eq[Date] =
    Eq.instance[Date]((a, b) => a.getTime === b.getTime)
}
