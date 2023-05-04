package part3datamanipulation

import scala.annotation.tailrec
import scala.util.Try

object DataValidation {

  import cats.data.Validated
  // Acts like an `Either` where left type is the undesirable type and right is desirable

  // Right value
  val validValue: Validated[String, Int] = Validated.valid(91)
  // Left value
  val invalidValue: Validated[String, Int] = Validated.invalid("Something went wrong...")
  // Similar to the ternary operator
  val testValue: Validated[String, Int] = Validated.cond(1343 > 12302, 1, "Nope!")

  // Why not use `Either`? Exercise time!
  // Exercise 1
  /*
    Define a function called `testNumber` that takes a number n and is checked against the following:
    - n must be prime
    - n must be non-negative
    - n <= 100
    - n must be even

    If n meets all these criteria, return a Right with that value.
    Otherwise, return a Left with all the criteria that n does not meet
   */

  // Solution
  def prime(n: Int): Boolean = {
    val abs = math.abs(n)
    if (abs <= 1) false else !(2 until abs - 1).exists(abs % _ == 0)
  }

  // Alternate prime function
  def testPrime(n: Int): Boolean = {
    @tailrec
    def loop(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && loop(d - 1)

    val abs = math.abs(n)
    if (abs <= 1) false else loop(abs / 2)
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val primeCheck = Either.cond(prime(n), n, List(s"$n is not prime"))
    val nonNegativeCheck = Either.cond(n >= 0, n, List(s"$n is not non-negative"))
    val lessThan100Check = Either.cond(n <= 100, n, List(s"$n is not less than 100"))
    val evenCheck = Either.cond(n % 2 == 0, n, List(s"$n is not even"))

    val errors = List(primeCheck, nonNegativeCheck, lessThan100Check, evenCheck).collect {
      case Left(value) => value
    }.flatten

    if (errors.isEmpty) Right(n) else Left(errors)
  }

  import cats.Semigroup
  import cats.instances.list._
  import cats.syntax.semigroup._
  implicit val MaxInt: Semigroup[Int] = Semigroup.instance(_.max(_))

  // As seen above, Either does not have a very good API for combining errors into one
  // Validated does
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(prime(n), n, List(s"$n is not prime")) |+|
      Validated.cond(n >= 0, n, List(s"$n is not non-negative")) |+|
      Validated.cond(n <= 100, n, List(s"$n is not less than 100")) |+|
      Validated.cond(n % 2 == 0, n, List(s"$n is not even"))

  // Some nice APIs in Validated
  // .andThen (chain Validated instances)
  validValue.andThen(_ => invalidValue)
  // .andThen is not .flatMap because .flatMap would not evaluate any further instances if one is invalid
  // So, .flatMap short-circuits the output

  // .ensure
  validValue.ensure(List("Something went wrong..."))(
    _ % 2 == 0
  ) // Converted to Invalid if value is not even

  // transform
  validValue.map(_ + 1)
  invalidValue.leftMap(_.length)
  validValue.bimap(_.length, _ + 2)

  // Interoperate with Standard Scala
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(3))

  val optionToValidated: Validated[List[String], Int] =
    Validated.fromOption(None, List("Nothing here..."))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  // And the other direction (except for Try because of Throwable)
  validValue.toOption
  invalidValue.toEither

  // Exercise 2: Form validation
  /*
    Implement `validateFrom` in `FormValidation` such that:
    - form contains "name", "email", "password"
    - All these fields must be specified
    - Name must not be blank
    - email must have an @
    - password must have >= 10 characters

    If user fails to meet any conditions, all errors must be shown. Otherwise, return "success!"
   */
  object FormValidation {
    type FormValidation[A] = Validated[List[String], A]

    def getValue(form: Map[String, String], field: String): FormValidation[String] =
      Validated.fromOption(form.get(field), List(s"Field $field must be specified"))

    def filter(str: String)(p: String => Boolean, error: String): FormValidation[String] =
      Validated.cond(p(str), str, List(error))

    def validateForm(form: Map[String, String]): FormValidation[String] = {
      val nameCheck =
        getValue(form, "name").andThen(filter(_)(_.trim.nonEmpty, "Name field cannot be blank"))

      val emailCheck = getValue(form, "email").andThen(filter(_)(_.contains('@'), "Invalid email"))
      val passwordCheck = getValue(form, "password").andThen(
        filter(_)(_.lengthCompare(10) >= 0, "Password must have at least 10 characters")
      )

      (nameCheck |+| emailCheck |+| passwordCheck).map(_ => "Registration successful")
    }
  }

  // Improved syntax
  import cats.syntax.validated._
  val anotherValidValue: Validated[List[String], Int] = 23.valid
  val anotherInvalidValue: Validated[List[String], Int] = List("Something went wrong...").invalid

  def main(args: Array[String]): Unit = {
    printlnWithSeparator("Checking for primality")
    (1 to 10).map(x => (x, prime(x))).foreach(println)
    printlnWithSeparator("")
    (1 to 10).map(x => (x, testPrime(x))).foreach(println)

    printlnWithSeparator("Exercise 1")
    println(testNumber(1))
    println("Using Validated")
    println(validateNumber(1))

    printlnWithSeparator("Exercise 2")
    val form = Map("name" -> "RZK", "email" -> "rzk@me.com", "password" -> "1234567890")
    println(FormValidation.validateForm(form))
    val invalidForm1 = form - "email" - "password"
    println(FormValidation.validateForm(invalidForm1))
    val invalidForm2 = form.updated("email", "rzk")
    println(FormValidation.validateForm(invalidForm2))

    printlnWithSeparator("Using Validated syntax")
    println(anotherValidValue)
    println(anotherInvalidValue)
  }
}
