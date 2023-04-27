package part2abstractMath

import scala.util.Try

object Monads {

  val numbers: List[Int] = List(1, 2, 3)
  val chars: List[Char] = List('a', 'b', 'c')

  // Exercise 1: Create all combinations of (number, char)
  val allCombinations: List[(Int, Char)] = numbers.flatMap(n => chars.map((n, _)))

  // ^== this is identical to this ==v
  val allCombinationsForYield: List[(Int, Char)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  // Exercise 2: Combination of options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')

  val allOptionCombinations: Option[(Int, Char)] = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // And the same for any other container types -- Try, Future, ...

  // Fundamental operations of Monads
  // - wrapping a value into a monadic value
  // - transform monadic value into other monadic value (flatMap)

//  trait MyMonad[M[_]] {
//    def pure[A](value: A): M[A]
//    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
//  }

  import cats.Monad
  import cats.instances.option._
  val optionMonad: Monad[Option] = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4)

  val transformedOption: Option[Int] =
    optionMonad.flatMap(Option("Bla"))(v => Try(v.toInt).toOption)

  // Similarly for lists
  import cats.instances.list._
  val listMonad: Monad[List] = Monad[List]
  val aList: List[Int] = listMonad.pure(4)
  val transformedList: List[Int] = listMonad.flatMap(aList)(v => List(v, v + 1))

  // General API (where Monads become useful)
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit M: Monad[M]): M[(A, B)] =
    M.flatMap(ma)(a => M.map(mb)((a, _)))

  // Extension methods (needs more than just cats.syntax.monad._)
  // For pure
  import cats.syntax.applicative._
  val anotherOption: Option[Int] = 2.pure[Option]
  val anotherList: List[Int] = 5.pure[List]

  // For flatMap
  import cats.syntax.flatMap._
  val anotherOptionTransformed: Option[Int] = anotherOption.flatMap(x => (x + 1).pure[Option])

  // Exercise 3: Implement map method in MyMonad
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  // Monads extend Functors
  import cats.syntax.functor._

  // Exercise 4: Shorter version of `getPairs`
  def getPairsShorter[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(anOption)
    println(transformedOption)
    println(aList)
    println(transformedList)

    println("\n======= Using Monads! =======")
    println(getPairs(numbers, chars))
    println(getPairs(numberOption, charOption))

    println("\n======= Using extension methods =======")
    println(getPairsShorter(numbers, chars))
    println(getPairsShorter(numberOption, charOption))
  }
}
