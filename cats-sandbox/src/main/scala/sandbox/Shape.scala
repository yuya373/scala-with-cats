class Shape
case class Circle(radius: Double) extends Shape

// Covariance

// scala> val circles: List[Circle] = List(Circle(3.0))
// circles: List[Circle] = List(Circle(3.0))

// scala> val shapes: List[Shape] = circles
// shapes: List[Shape] = List(Circle(3.0))

object Variance {
  // Contravariance
  trait JsonWriter[-A] {
    def write(value: A): Json
  }

  object JsonWriterInstances {
    implicit val shapeWriter: JsonWriter[Shape] = new JsonWriter[Shape] {
      def write(value: Shape) = JsString("Shape")
    }
    implicit val circleWriter: JsonWriter[Circle] = new JsonWriter[Circle] {
      def write(value: Circle) = JsString("Circle")
    }
  }

  object JsonWriterOps {
    def format[A](value: A)(implicit writer: JsonWriter[A]): Json =
      writer.write(value)
  }
  // Circle is subtype of Shape
  // F[Shape] is subtype of F[Circle]

  // scala> import JsonWriterInstances._
  // import JsonWriterInstances._

  // scala> val circle = Circle(3.0)
  // circle: Circle = Circle(3.0)

  // scala> JsonWriterOps.format(circle)(shapeWriter)
  // res1: Json = JsString(Shape)

  // scala> JsonWriterOps.format(circle)(circleWriter)
  // res2: Json = JsString(Circle)

  // scala> val shape = new Shape
  // shape: Shape = Shape@17da4d27

  // scala> JsonWriterOps.format(shape)(shapeWriter)
  // res3: Json = JsString(Shape)

  // scala> JsonWriterOps.format(shape)(circleWriter)
  // <console>:26: error: type mismatch;
  //  found   : JsonWriter[Circle]
  //  required: JsonWriter[Shape]
  //        JsonWriterOps.format(shape)(circleWriter)
  //                                    ^


  // if Contravariant, supertype instance is used, subtype is not used.
  // scala> import JsonWriterInstances._
  // import JsonWriterInstances._

  // scala> val circle = Circle(3.0)
  // circle: Circle = Circle(3.0)

  // scala> JsonWriterOps.format(circle)
  // res8: Json = JsString(Shape)

  // specify instance to use Subtype instance
  // scala> JsonWriterOps.format(circle)(circleWriter)
  // res9: Json = JsString(Circle)
}
