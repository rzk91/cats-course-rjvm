package part3datamanipulation

object FunctionalState {

  type MyState[S, A] = S => (S, A)
  // S -> state; A -> desired value

  import cats.data.State
  val countAndSay: State[Int, String] = State(count => (count + 1, s"Counted $count"))

  // To execute, use `.run`
  val (_, _) = countAndSay.run(10).value // .run returns an `Eval`, so use .value to get output

  // Example of how state can be managed in a functional way
  // Iterative (ugh!)
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10; obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5; obtained $a"

  // Using pure FP with `State`
  val firstTransformation: State[Int, String] =
    State((s: Int) => (s + 1, s"Added 1 to 10; obtained $s"))

  val secondTransformation: State[Int, String] =
    State((s: Int) => (s * 5, s"Multiplied with 5; obtained $s"))

  val compositeTransformation: State[Int, (String, String)] =
    firstTransformation.flatMap(r1 => secondTransformation.map((r1, _)))

  val compositeTransformationFor: State[Int, (String, String)] = for {
    r1 <- firstTransformation
    r2 <- secondTransformation
  } yield (r1, r2)

  // Why not just use functions, instead? TLDR; becomes too cumbersome and confusing upon multiple chain operations
  val func1: Int => (Int, String) = (s: Int) => (s + 1, s"Added 1 to 10; obtained $s")
  val func2: Int => (Int, String) = (s: Int) => (s * 5, s"Multiplied with 5; obtained $s")

  val compositeFunc: Int => (String, (Int, String)) = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  // Exercise 1: Online store that sells guitars;
  // Create a function `addedToCar` that stores a new item in the shopping cart and returns the new state
  case class ShoppingCart(items: List[String], total: Double) {
    def add(item: String, price: Double): ShoppingCart = ShoppingCart(item :: items, total + price)
  }

  // Solution
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State(cart => (cart.add(item, price), cart.total + price))

  val myCart: State[ShoppingCart, Double] = for {
    _     <- addToCart("Fender", 500)
    _     <- addToCart("Ibanez", 700)
    _     <- addToCart("Schecter", 1000)
    total <- addToCart("Cables", 9)
  } yield total

  // Exercise 2: "pure mental gymnastics!"
  /*
   Create the following:
    - an `inspect` method that will not change state but output f(a) as value
    - a `get` method that return the same value and make no changes
    - a `set` method that returns Unit and sets state to the supplied argument
    - a `modify` method that returns Unit and modifies state to f(state)
   */

  // Solution
  def inspect[A, B](f: A => B): State[A, B] = State(a => (a, f(a)))
  def get[A]: State[A, A] = State(a => (a, a))
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))
  def modify[A](f: A => A): State[A, Unit] = State(a => (f(a), ()))

  // These methods are already implemented in State's companion object (duh!)
  //  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get
    _ <- set(a + 10)
    b <- get
    _ <- modify[Int](_ + 50)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  case class Trick(orig: Int, curr: Int) {
    def update(f: Int => Int): Trick = copy(curr = f(curr))
    def subtractOriginal: Trick = copy(curr = curr - orig)
  }

  object Trick {
    def apply(v: Int): Trick = new Trick(v, v)
  }

  val mathTrick: State[Trick, Int] = for {
    init <- get
    _ <- set(init.update(_ * 2))
    _ <- modify[Trick](_.update(_ + 50))
    _ <- modify[Trick](_.update(_ / 2))
    _ <- modify[Trick](_.subtractOriginal)
    out <- get
  } yield out.curr

  def main(args: Array[String]): Unit = {
    // State is properly separated from value; easier for chained computations
    println(compositeTransformationFor.run(10).value)

    // State is nested somewhere in the output and will get even more nested if we perform another transformation (map/flatMap)
    println(compositeFunc(10))

    printlnWithSeparator("Exercise 1")
    println(myCart.run(ShoppingCart(List.empty, 0)).value)

    printlnWithSeparator("Exercise 2")
    println(program.run(1).value)

    printlnWithSeparator("Math trick")
    val start = 5
    val expectedOutput = 25
    println(s"Starting value: $start")
    val (trickState, trickOutput) = mathTrick.run(Trick(start)).value
    println((trickState, trickOutput))
    println(expectedOutput == trickOutput)
  }
}
