package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives /*Apply*/ {

  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F] {

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
//      val functionWrapper: F[B => (A, B)] = map(fa)(a => b => (a, b))
//      ap(functionWrapper)(fb)
      ap(map(fa)(a => (b: B) => (a, b)))(fb)

    def ap[B, T](ff: F[B => T])(fb: F[B]): F[T] // Fundamental method

    def mapN2[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] =
      ap(map(tuple._1)(a => (b: B) => f(a, b)))(tuple._2) // Looks shite...

    def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] =
      map(product(tuple._1, tuple._2)) { case (a, b) => f(a, b) } // Much better
  }

  trait MyApplicative[F[_]] extends MyApply[F] {
    def pure[A](x: A): F[A] // fundamental method
  }

  import cats.Apply
  import cats.instances.option._
  val applyOption: Apply[Option] = Apply[Option]
  val funcApp: Option[Int] = applyOption.ap(Option((x: Int) => x + 1))(Option(2))

  import cats.syntax.apply._
  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuples: Option[(Int, Int, Int)] = tupleOfOptions.tupled // Some((1, 2, 3))

  val sumOption: Option[Int] = tupleOfOptions.mapN(_ + _ + _)

  // Exercise 1: Implement mapN method in `MyApply`

  def main(args: Array[String]): Unit = {
    println(funcApp)
    println(sumOption)
  }
}
