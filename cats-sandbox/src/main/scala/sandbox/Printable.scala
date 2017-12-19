trait Printable[A] {
  def format(value: A): String
  def print(value: A): Unit = println(format(value))
}

object PrintableInstances {
  implicit val stringPrinter: Printable[String] = new Printable[String] {
    def format(value: String) = value
  }

  implicit val intPrinter: Printable[Int] = new Printable[Int] {
    def format(value: Int) = value.toString()
  }

  implicit val catPrinter: Printable[Cat] = new Printable[Cat] {
    def format(value: Cat) =
      s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}

object Printable {
  def format[A](value: A)(implicit printer: Printable[A]): String =
    printer.format(value)

  def print[A](value: A)(implicit printer: Printable[A]): Unit = {
    val str = printer.format(value)
    println(str)
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printer: Printable[A]): String =
      printer.format(value)

    def print(implicit printer: Printable[A]): Unit = {
      printer.print(value)
    }
  }
}
