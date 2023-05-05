package part4typeclasses

import cats.data.Validated

object Semigroupals {

  trait MySemigroupal[F[_]] {
    // Basically a tuple of two values instead of a combination
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // Implicit Semigroupal[Option]
  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]

  val tupledOption: Option[(Int, String)] =
    optionSemigroupal.product(Option(123), Option("Something"))
  // Returns Option((123, "Something"))
  val tupleWithNone: Option[(Int, Nothing)] = optionSemigroupal.product(Option(123), None)
  // Returns None

//  import cats.instances.future._
//
//  val tupledFuture: Future[(Int, String)] =
//    Semigroupal[Future].product(Future(42), Future("Some value"))

  // Why are Semigroupals necessary?
  import cats.instances.list._ // implicit Monad[List]
  val listSemigroupal: Semigroupal[List] = Semigroupal[List]
  val numberList: List[Int] = List(1, 2)
  val stringList: List[String] = List("a", "b")

  val tupledList
    : List[(Int, String)] = Semigroupal[List].product(numberList, stringList) // Cartesian product!

  // Exercise 1: Implement product using Monads
  import cats.Monad

  def productWithMonadsPure[F[_], A, B](fa: F[A], fb: F[B])(implicit M: Monad[F]): F[(A, B)] =
    M.flatMap(fa)(a => M.map(fb)(b => (a, b)))

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit M: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // Monads extend Semigroupals!
  // Semigroupals are needed for performing map/flatMap-like operations without having to follow monadic laws
  // Associativity law: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
  // For example: Validated!

  type ErrorsOr[A] = Validated[List[String], A]

  val validatedSemigroupal
    : Semigroupal[ErrorsOr] = Semigroupal[ErrorsOr] // requires implicit Semigroupal[List]

  val invalidsCombination: ErrorsOr[(Nothing, Nothing)] = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  import cats.instances.either._ // implicit Monad[Either]
  type EitherErrorsOr[A] = Either[List[String], A]
  val eitherSemigroupal: Semigroupal[EitherErrorsOr] = Semigroupal[EitherErrorsOr]

  val eitherCombination: EitherErrorsOr[(Nothing, Nothing)] =
    eitherSemigroupal.product( // implemented in terms of map/flatMap
      Left(List("Something wrong", "Something else wrong")),
      Left(List("This can't be right"))
    )
  // Last error is not propagated because flatMap short-circuits the computation

  // Exercise 2: Define a Semigroupal[List] that does a zip
  val listSemigroupalZip: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(tupledOption)
    println(tupleWithNone)

    println(invalidsCombination)
    println(eitherCombination)

    printlnWithSeparator("Exercise 2")
    println(tupledList)
    println(listSemigroupalZip.product(numberList, stringList))
  }
}
