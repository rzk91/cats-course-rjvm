package part5alienbits

object Kleislis {

  val f1: Int => Option[String] = x => if (x % 2 == 0) Option(s"$x is even") else None
  val f2: Int => Option[Int] = x => Option(x * 3)

  // f3 = f2 andThen f1
  val plainF1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  val plainF2: Int => Int = x => x * 3
  val plainF3: Int => String = plainF2 andThen plainF1 // or plainF1 compose plainF2
  // But this is not possible with f1 and f2 because they return Option
  // This will fail
  // val f3 = f2 andThen f1

  import cats.data.Kleisli // Named after a Swiss mathematician
  val f1K: Kleisli[Option, Int, String] = Kleisli[Option, Int, String](f1)
  val f2K: Kleisli[Option, Int, Int] = Kleisli(f2) // Kleisli[Option, Int, Int]
  val f3K: Kleisli[Option, Int, String] = f2K andThen f1K // andThen relies on FlatMap[Option]

  // Convenience methods
  val multiply: Kleisli[Option, Int, Int] = f2K.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain: Kleisli[Option, Int, String] = f2K.flatMap(_ => f1K)

  // Exercise 1
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // This is basically Reader[A, B] B-)

  val times2: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](_ * 2)
  val plus4: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](_ + 4)
  val composed: Kleisli[Id, Int, Int] = times2.flatMap(t2 => plus4.map(_ + t2))
  val composedFor: Kleisli[Id, Int, Int] = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  def main(args: Array[String]): Unit = {
    println(f3K(5))
    println(f3K(6))
    println(chain(10))

    println(composedFor(3)) // (3 * 2) + (3 + 4)
  }
}
