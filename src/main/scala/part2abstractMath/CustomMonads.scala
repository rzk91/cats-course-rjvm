package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    def pure[A](x: A): Option[A] = Option(x)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    // Mainly for iteration purposes
    // Has to be tail-recursive and never stack-overflow
    @tailrec
    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None           => None
      case Some(Left(v))  => tailRecM(v)(f)
      case Some(Right(v)) => Option(v)
    }
  }

  // Exercise 1: Define a Monad for the Identity tag
  type Identity[T] = T
  val someNumber: Identity[Int] = 1

  implicit object IdentityMonad extends Monad[Identity] {
    def pure[A](x: A): Identity[A] = x
    def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v)  => tailRecM(v)(f)
      case Right(v) => v
    }
  }

  // Exercise 2: Define a Monad for a binary tree
  sealed trait Tree[+A]

  object Tree {
    def leaf[A](value: A): Tree[A] = Leaf(value)
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  }
  import Tree._

  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {
    def pure[A](x: A): Tree[A] = leaf(x)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
    }

    def flatMapTailrec[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      @tailrec
      def loop(todo: List[Tree[A]], expanded: List[Tree[A]], done: List[Tree[B]]): Tree[B] =
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(value) => loop(todo.tail, expanded, f(value) :: done)
            case tree @ Branch(left, right) if !expanded.contains(tree) =>
              loop(right :: left :: todo, expanded :+ tree, done)
            case _ =>
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = branch(newLeft, newRight)
              loop(todo.tail, expanded, newBranch :: done.drop(2))
          }

      loop(List(fa), List.empty, List.empty)
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(value))   => stackRec(f(value))
        case Leaf(Right(value))  => leaf(value)
        case Branch(left, right) => branch(stackRec(left), stackRec(right))
      }

      @tailrec
      def tailRec(
        todo: List[Tree[Either[A, B]]],
        expanded: List[Tree[Either[A, B]]],
        done: List[Tree[B]]
      ): Tree[B] =
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(Left(value))  => tailRec(f(value) :: todo.tail, expanded, done)
            case Leaf(Right(value)) => tailRec(todo.tail, expanded, leaf(value) :: done)
            case tree @ Branch(left, right) if !expanded.contains(tree) =>
              tailRec(left :: right :: todo, expanded :+ tree, done)
            case _ =>
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = branch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
          }

//      stackRec(f(a))
      tailRec(List(f(a)), List.empty, List.empty)
    }

  }

  def main(args: Array[String]): Unit = {
    println(someNumber)

    val tree = branch(leaf(10), leaf(20))
    val transformed = TreeMonad.flatMap(tree)(v => branch(leaf(v * 2), leaf(v * 3)))
    val transformed2 = TreeMonad.flatMapTailrec(tree)(v => branch(leaf(v * 2), leaf(v * 3)))
    println(transformed)
    println(transformed2)

  }
}
