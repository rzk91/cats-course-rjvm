package part5alienbits

import cats.kernel.Monoid

object ContravariantFunctors {

  trait Format[T] { self =>
    def format(value: T): String
    def contramap[A](func: A => T): Format[A] = (value: A) => self.format(func(value))
  }

  def format[T](value: T)(implicit f: Format[T]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = s""""$value""""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // Problem: given Format[CustomType], can we have Format[Option[CustomType]]
  // One possible way
  implicit def getOptionFormat[A](implicit f: Format[A], m: Monoid[A]): Format[Option[A]] =
    f.contramap[Option[A]](_.getOrElse(m.empty))

  /*
    How does the compiler compile Option(Option(3))?
    - We have IntFormat in scope
    - fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get) // first get
    - fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) // second get

      So, fo2 = IntFormat
            .contramap[Option[Int]](_.get)
            .contramap[Option[Option[Int]]](_.get)

    Order of operations:
     - fo2.format(Option(Option(3))) = fo1.format(secondGet(Option(Option(3)))
      = IntFormat.format(firstGet(secondGet(Option(Option(3)))))

    Second get -> first get -> int format

    So, in reverse order of how it's written (i.e., stack order)
    Hence, the name. Unlike map that does transformations in sequence, contramap applies them in reverse sequence.

    Type classes like Format are called contravariant type classes (not to confuse with contravariance)

    contramap is used to *derive* instances of a type class given a transformation from one to another
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._ // implicit Show[Int]

  val showInts: Show[Int] = Show[Int]
  val showOptions: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Bla"))
    println(format(1))
    println(format(true))

    println(format(Option(1)))
    println(format(Option(Option(3))))
//    println(format(None)) // Doesn't work... no implicits found
//    println(format(Option.empty[Int])) // Fails (because of _.get)
  }
}
