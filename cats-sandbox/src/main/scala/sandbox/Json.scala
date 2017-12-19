sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json
// Type Class
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)
// Type Class Instance
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(value: String): Json = JsString(value)
  }

  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
    def write(value: Person): Json = JsObject(Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    ))
  }

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(option: Option[A]): Json = option match {
        case Some(value) => writer.write(value)
        case None => JsNull
      }
    }
}
// Type Class Interface: Interface Objects
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}
// Type Class Interface: Interface Syntax
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }
}
