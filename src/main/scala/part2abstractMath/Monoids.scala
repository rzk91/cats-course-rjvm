package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.eq._
  import cats.syntax.monoid._

  val numbers: List[Int] = (1 to 1000).toList
  // |+| is associative --> a + b = b + a
  val sumLeft: Int = numbers.foldLeft(0)(_ |+| _)
  val sumRight: Int = numbers.foldRight(0)(_ |+| _)

  // Define a general API
//  def combineFold[A](list: List[A])(implicit semigroup: Semigroup[A]): A =
//    list.foldLeft(???)(_ |+| _)
  // ??? should be the empty version (starting/neutral/zero value) of A

  // Introducing Monoids
  import cats.Monoid
  val intMonoid: Monoid[Int] = Monoid[Int]
  val combineInt: Int = intMonoid.combine(100, 200) // 300
  val zero: Int = intMonoid.empty                   // 0

  import cats.instances.string._
  val stringMonoid: Monoid[String] = Monoid[String]
  val combineString: String = stringMonoid.combine("What's ", "up!")
  val emptyString: String = stringMonoid.empty

  import cats.instances.option._
  val optionMonoid: Monoid[Option[Int]] = Monoid[Option[Int]]
  val emptyOption: Option[Int] = optionMonoid.empty

  val combineOptions
    : Option[Int] = optionMonoid.combine(Option(2), Option.empty[Int]) // Sums 2 and "0" to give Some(2)!

  // Extension methods
  val combineOptions2: Option[Int] = Option(3) |+| Option(5)

  // Define a general API (also, exercise 1)
  def combineFold[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldLeft(monoid.empty)(_ |+| _)

  // Exercise 2: Combine list of phonebooks as Map[String, Int]
  val phonebooks: List[Map[String, Int]] = List(
    Map(
      "Alice" -> 234,
      "Bob"   -> 656
    ),
    Map(
      "Charlie" -> 232,
      "Daniel"  -> 768
    ),
    Map(
      "Tina" -> 123
    )
  )

  import cats.instances.map._
  val hugePhonebook: Map[String, Int] = combineFold(phonebooks)

  // Exercise 3: Shopping cart and online starts with Monoids
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart](
      emptyValue = ShoppingCart(List(), 0.0),
      cmb = (c1, c2) => ShoppingCart(c1.items ++ c2.items, c1.total + c2.total)
    )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft === sumRight)
    println(s"Empty string: $emptyString")
    println(s"Combined string: $combineString")

    println(s"Combined options 1: $combineOptions")
    println(s"Combined options 2: $combineOptions2")

    println("\n=== Using monoid syntax ===")
    println(combineFold(numbers))
    println(combineFold(List("I ", "like ", "ice cream!")))

    println(hugePhonebook)

    val shoppingCarts = List(
      ShoppingCart(List("Shirt", "Pant", "Football"), 50.0),
      ShoppingCart(List("Jersey", "Shoes"), 100.0),
      ShoppingCart(List("Watch"), 1000)
    )
    println(checkout(shoppingCarts))
  }

}
