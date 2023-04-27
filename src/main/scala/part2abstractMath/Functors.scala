package part2abstractMath

import scala.util.Try

object Functors {

  // Some containers
  val numbers: List[Int] = List(1, 2, 3)
  val maybeInt: Option[Int] = Option(2)
  val triedInt: Try[Int] = Try(4)

  // Simple definition of Functor
  trait MyFunctor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor: Functor[List] = Functor[List]
  val incrementedNumbers: List[Int] = listFunctor.map(numbers)(_ + 1) // List(2, 3, 4)

  import cats.instances.option._
  val optionFunctor: Functor[Option] = Functor[Option]
  val incrementedOption: Option[Int] = optionFunctor.map(maybeInt)(_ + 1) // Some(3)

  import cats.instances.try_._
  val optionTry: Functor[Try] = Functor[Try]
  val incrementedTry: Try[Int] = optionTry.map(triedInt)(_ + 1) // Success(5)

  // Functors are important when we want to generalise an API
  // So, instead of this...
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(opt: Option[Int]): Option[Int] = opt.map(_ * 10)
  def do10xTry(tried: Try[Int]): Try[Int] = tried.map(_ * 10)

  // ... create a general API
  def do10x[F[_]](ints: F[Int])(implicit F: Functor[F]): F[Int] = F.map(ints)(_ * 10)

  // Exercise 1: Define a functor for a binary tree
  // Branch only has left and right trees and no value (unlike in the video)
  // Also, trait is sealed for exhaustive pattern matching
  sealed trait Tree[+T]
  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](left: Tree[T], right: Tree[T]): Tree[T] = Branch(left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {

    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  import cats.syntax.functor._
  def shorterDo10x[F[_]: Functor](ints: F[Int]): F[Int] = ints.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println("======= Functors! ========")
    println(incrementedNumbers)
    println(incrementedOption)
    println(incrementedTry)

    println("\n======== Do 10x =======")
    println(do10x(numbers))
    println(do10x(maybeInt))
    println(do10x(triedInt))

    import Tree._
    val tree = branch(leaf(1), branch(leaf(2), leaf(3)))
    // Can also do without smart constructors (i.e. importing Tree._) but then we need to specify the type `Tree` when instantiating
    // This is because we only have an implicit functor for Tree and since cats is invariant, it won't find it for Branch
    // val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    println(do10x(tree))

    // Using extension methods!
    println("======= Using extension methods! ========")
    println(tree.map(_ * 10))
    println(shorterDo10x(numbers))
    println(shorterDo10x(maybeInt))
    println(shorterDo10x(tree))
  }
}
