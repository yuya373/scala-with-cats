trait Printable[A] {
  // use a `self` alias to distinguish the outer and inner Printables
  self =>
  def format(value: A): String
  def print(value: A): Unit = println(format(value))
  def contramap[B](f: B => A): Printable[B] = new Printable[B] {
    def format(value: B): String = {
      self.format(f(value))
    }
  }
}

final case class Box[A](value: A)

object PrintableInstances {
  implicit val stringPrinter: Printable[String] = new Printable[String] {
    def format(value: String) = s""""${value}""""
  }

  implicit val intPrinter: Printable[Int] = new Printable[Int] {
    def format(value: Int) = value.toString()
  }

  implicit val catPrinter: Printable[Cat] = new Printable[Cat] {
    def format(value: Cat) =
      s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }

  implicit val booleanPrinter: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean) = if (value) "yes" else "no"
  }

  implicit def boxPrinter[A](implicit p: Printable[A]): Printable[Box[A]] =
    // F[B].contramap(A => B): F[A]
    // Printable[A].contramap(Box[A] => A): Printable[Box[A]]
    // transformation from Printable[A] to Printable[Box[A]]
    // prepending (f: Box[A] => A) to Printable[A]
    p.contramap((box: Box[A]) => box.value)
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
