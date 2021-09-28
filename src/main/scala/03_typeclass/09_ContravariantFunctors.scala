package typeclass

import cats.kernel.Monoid

object ContravariantFunctors {

  trait Format[T] { self => // contravariant type classes
    def format(value: T): String

    def contramap[A](f: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(f(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  import cats.instances.option._ // Monoid[Option]

  // problem: given Format[MyType], can we have a Format[Option[MyType]] ?
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  /**
   *    IntFormat
   *    fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get)
   *    fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get)
   *
   *    fo2 = InFormat
   *        .contramap[Option[Int]](_.get)         // first get
   *        .contramap[Option[Option[Int]]](_.get) // second get
   *
   *    order: REVERSE from the written order
   *    - second get
   *    - first get
   *    - format of Int
   *
   *    Map applies transformation in sequence
   *    Contramap applies transformations in REVERSE sequence
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._ // implicit Show[Int]

  val showInts                      = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._ // extension method

  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Nothing weird so far"))
    println(format(42))
    println(format(true))
    println(format(Option(42)))
    println(format(Option(Option(Option(22)))))
  }
}
