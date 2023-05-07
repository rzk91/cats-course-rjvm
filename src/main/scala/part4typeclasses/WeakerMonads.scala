package part4typeclasses

import cats.{Applicative, Apply}

object WeakerMonads /*FlatMap*/ {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // Exercise 1: Implement ap in FlatMap
    def ap[A, B](mf: M[A => B])(ma: M[A]): M[B] =
      flatMap(ma)(a => map(mf)(aToB => aToB(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  // Just like we did in the Monads lecture
  // But instead of Monad as the constraint on M, we use a "weaker Monad" --> FlatMap
  def getPairs[M[_]: FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)
}
