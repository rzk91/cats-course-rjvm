package part4typeclasses

import cats.{Eval, Monoid}

object Folding {

  // Exercise 1 (right away!): Implement these methods in terms of foldLeft/-Right
  object ListExercises {

    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B])((acc, a) => acc :+ f(a))

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((acc, a) => acc ++ f(a))

    def flatMap2[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((acc, a) => acc.foldRight(f(a))(_ :: _))

    def filter[A](list: List[A])(p: A => Boolean): List[A] =
      list.foldLeft(List.empty[A])((acc, a) => if (p(a)) acc :+ a else acc)

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list._

  val foldableList: Foldable[List] = Foldable[List]
  val foldedList: Int = foldableList.foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._
  val foldableOption: Foldable[Option] = Foldable[Option]
  val foldedOption: Int = foldableOption.foldLeft(Option(2), 4)(_ + _) // 6

  // foldRight is stack-safe because of Eval!
  val rightFoldedList: Eval[Int] = foldableList.foldRight(List(1, 2, 3), Eval.now(0)) {
    (num, eval) => eval.map(_ + num)
  }

  // combineAll in Foldable
  val anotherSum: Int = foldableList.combineAll(List(1, 2, 3)) // implicit Monoid[Int]

  import cats.instances.string._
  val mappedConcat: String = foldableList.foldMap(List(1, 2, 3))(_.toString)

  // Deeply nested instances
  val intsNested: List[Vector[Int]] = List(Vector(1, 2), Vector(3))
  val foldedNestedInts: Int = (foldableList compose Foldable[Vector]).combineAll(intsNested)

  // Extension methods
  import cats.syntax.foldable._
  val sum3: Int = List(1, 2, 3).combineAll // requires Foldable[List] and Monoid[Int]
  val mappedConcat2: String = List(1, 2, 3).foldMap(_.toString) // requires Foldable[List] and Monoid[String]


  def main(args: Array[String]): Unit = {
    printlnWithSeparator("Exercise 1")
    import ListExercises._
    val list = (1 to 10).toList
    println(map(list)(_ + 1))
    println(flatMap(list)(x => (1 to x).toList))
    println(flatMap2(list)(x => (1 to x).toList))
    println(filter(list)(_ % 2 == 0))

    import cats.instances.int._
    println(combineAll(list))

    printlnWithSeparator("Foldable")
    println(s"Folded list: $foldedList")
    println(s"Folded option: $foldedOption")
    println(s"Right-folded list: ${rightFoldedList.value}")
    println(s"Combined list: $anotherSum")
    println(s"Folded nested ints: $foldedNestedInts")
    println(s"Mapped concat: $mappedConcat")
  }
}
