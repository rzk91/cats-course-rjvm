package part4typeclasses

object Applicatives {

  // Applicatives = Functors + pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative: Applicative[List] = Applicative[List]

  val list: List[Int] = listApplicative.pure(2)

  import cats.instances.option._
  val optionApplicative: Applicative[Option] = Applicative[Option]
  val anOption: Option[Int] = optionApplicative.pure(3)

  // Pure extension method
  import cats.syntax.applicative._
  val pureList: List[Int] = 2.pure[List]
  val pureOption: Option[Int] = 4.pure[Option]

  // Monads extend Applicatives (they inherit the pure method from there)
  // Applicatives extend Functors (.map available)

  // Most Applicatives are Monads because most of them need chaining (map/flatMap)
  // One notable exception is Validated (just as mentioned in Semigroupal)
  import cats.data.Validated
  type ErrorsOr[A] = Validated[List[String], A]
  val validValue: ErrorsOr[Int] = Validated.valid(42) // pure
  val invalidValue: ErrorsOr[Int] = Validated.invalid(List("Something went wrong..."))
  val modifiedValidated: ErrorsOr[Int] = validValue.map(_ + 1) // map

  val validatedApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]

  // Exercise 1: Thought experiment; define product using an implicit Applicative method (spoiler alert! Not possible -_-)
  // Instead, we need another helper method
  // Introducing ap (stands for apply)
//  def ap[W[_], B, TUPLE](wf: W[B => TUPLE])(wa: W[B]): W[TUPLE] = ??? // Assume this is implemented
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit A: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = A.map(wa)(a => (b: B) => (a, b))
    A.ap(functionWrapper)(wb)
  }

  // Applicative extends Semigroupal using the ap method
}
