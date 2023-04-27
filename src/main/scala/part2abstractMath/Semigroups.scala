package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of same type
  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombination: Int = naturalIntSemigroup.combine(1, 2) // Addition

  import cats.instances.string._
  val naturalStringSemigroup: Semigroup[String] = Semigroup[String]

  val stringCombination
    : String = naturalStringSemigroup.combine("Hello ", "world!") // Concatenation

  // Specific APIs
  def reduceInts(list: List[Int]): Int =
    list.reduce(naturalIntSemigroup.combine) // Same as list.reduce(_ + _)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // Generic API
  def reduceThings[A](list: List[A])(implicit semigroup: Semigroup[A]): A =
    list.reduce(semigroup.combine)

  // Exercise 1: Support a new type
  case class Expense(id: Long, amount: Double)

  // My solution
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (e1, e2) =>
    Expense(e1.id.max(e2.id), e1.amount + e2.amount)
  }

  // Extension methods for Semigroup -- |+|
  import cats.syntax.semigroup._
  val intSum: Int = 2 |+| 3 // Works because implicit Semigroup[Int] is in scope
  val stringSum: String = "Scala " |+| "is " |+| "awesome!"
  val expenseSum: Expense = Expense(10, 300) |+| Expense(20, 500)

  val normalAbstractSum: String = 2 + "something" // because of any2stringadd implicit conversion!
  val anotherNormalAbstractSum: String =
    Expense(20, 300) + "bla" // Also works because of any2stringadd!
  // val doesNotWork = 2 |+| "something" // Compiler error
  // val alsoDoesNotWork = Expense(20, 400) |+| "bla" // Error

  // Using extensions
  object CombinedLists {

    implicit class ListOps[A: Semigroup](private val list: List[A]) {
      def reduceAll(implicit semigroup: Semigroup[A]): A = list.reduce(semigroup.combine)
      def reduceAll2: A = list.reduce(_ |+| _)
    }
  }

  // Exercise 2: Implement reduceThings with combination function
  // And solution
  def reduceThings2[A: Semigroup](list: List[A]): A = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    println("\n===== Using specific APIs =====")
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))

    val strings = List("a, ", "b, ", "c, ", "d, ", "e!")
    println(reduceStrings(strings))

    // Generic APIs
    println("\n===== Using generic API =====")
    println(reduceThings(numbers))
    println(reduceThings(strings))
    import cats.instances.option._
    val numberOptions: List[Option[Int]] = numbers.map(Option(_))
    println(reduceThings(numberOptions))

    val expenses = List(Expense(1, 9.99), Expense(2, 5.99), Expense(3, 19.99))
    println(reduceThings(expenses))

    // Using extension method on List
    import CombinedLists._
    println("\n===== Using List extension method =====")
    println(numbers.reduceAll)
    println(strings.reduceAll2)
    println(numberOptions.reduceAll)
    println(expenses.reduceAll2)
  }
}
