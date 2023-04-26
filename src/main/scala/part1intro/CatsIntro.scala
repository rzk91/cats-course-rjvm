package part1intro

object CatsIntro {

  // Eq
  // Because why allow comparison of different types in the first place?
  2 == "string" // Unnecessary comparison -> obviously false

  // Type class import
  import cats.Eq

  // Import TC instances for the required types
  import cats.instances.int._

  // Use TC API
  val intEquality: Eq[Int] = Eq[Int]
  intEquality.eqv(1, 3) // Works
  // intEquality.eqv(1, "a string") // Doesn't work!

  // Use extension methods to make it look nice
  import cats.syntax.eq._
  2 === 3 // Works (returns false)
  // 2 === "something" // Doesn't work!
  // 2 =!= "something" // Also doesn't work
  1 eqv 3 // Another way of writing it, works (also returns false)

  // Extending a TC operation to composite types
  // val listComparison = List(2) === List(4) // Doesn't work just yet

  import cats.instances.list._
  List(2, 3) === List(3, 4) // Works (returns false)
  // List("a", "b") === List("b", "c") // Doesn't work because Eq[List[String]] is not available in scope

  import cats.instances.string._
  List("a", "b") === List("b", "c") // Works now because `string` is in scope

  // Custom objects
  case class ToyCar(model: String, price: Double) // Generally, don't use Double for currency
  import cats.instances.double._
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] {
    (car1, car2) => car1.price === car2.price
  }

  val compareTwoToyCars: Boolean = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99) // Works because of implicit val

  // Take-away
  // Use the following imports to use a specific type class from Cats
  // import cats.<TC>
  // import cats.instances.<T>._
  // import cats.syntax.<TC>._

  // Another easier way if you don't want to import TC instances for all types/don't know where they are/whatever
  // import cats._
  // import cats.implicits._

  def main(args: Array[String]): Unit = {
    println(s"Comparing two ToyCars: $compareTwoToyCars")
  }
}
