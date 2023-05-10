package part5alienbits

import cats.kernel.Monoid

object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  // How can we support caesarCypher for other types: ints, double, Option[String], ...?

  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  // Exercise 1: Support Option[String]
  // And exercise 2: support Option[A]
  implicit def optionCrypto[A](implicit crypto: Crypto[A], m: Monoid[A]): Crypto[Option[A]] =
    crypto.imap(_.getOrElse(m.empty), Option(_))

  import cats.Invariant
  import cats.Show
  val showStrings: Show[String] = Show[String]
  val showOptionStrings: Show[Option[String]] = Invariant[Show].imap(showStrings)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._
  val showOptionStringsShorter: Show[Option[String]] = showStrings.imap(Option(_))(_.getOrElse(""))

  // Exercise 3: Establishing connection between contravariant and invariant and functor
  // What's the relationship? My answer: Invariant is the super-type to both of them
  trait MyInvariant[F[_]] {
    def imap[A, B](fa: F[A])(f: A => B)(b: B => A): F[B]
  }

  trait MyContravariant[F[_]] extends MyInvariant[F] {
    def contramap[A, B](fa: F[A])(b: B => A): F[B]

    override def imap[A, B](fa: F[A])(f: A => B)(b: B => A): F[B] = contramap(fa)(b)
  }

  trait MyFunctor[F[_]] extends MyInvariant[F] { // Covariant
    def map[A, B](fa: F[A])(f: A => B): F[B]

    override def imap[A, B](fa: F[A])(f: A => B)(b: B => A): F[B] = map(fa)(f)
  }

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Encrypt this")
    val decrypted = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)

    val encryptedDouble = encrypt(math.Pi)
    val decryptedDouble = decrypt[Double](encryptedDouble)

    println(encryptedDouble)
    println(decryptedDouble)

    println("====== Exercise 1 =======")
    val encryptedOption = encrypt(Option("Encrypt this"))
    val decryptedOption = decrypt[Option[String]](encryptedOption)
    println(encryptedOption)
    println(decryptedOption)

    println("====== Exercise 2 =======")
    val encryptedOptionDouble = encrypt(Option(math.Pi))
    val decryptedOptionDouble = decrypt[Option[Double]](encryptedOptionDouble)
    println(encryptedOptionDouble)
    println(decryptedOptionDouble)
  }
}
