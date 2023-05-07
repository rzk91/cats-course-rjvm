package part4typeclasses

import cats.{Applicative, Monad}

import scala.concurrent.Future
import scala.util.Try

object ErrorHandling {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A] // E does not have actually be a JVM error; can be anything
    def handleErrorWith[A](ma: M[A])(f: E => M[A]): M[A]
    def handleError[A](ma: M[A])(f: E => A): M[A] = handleErrorWith(ma)(e => pure(f(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(p: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._      // implicit MonadError
  type ErrorOr[A] = Either[String, A] // here, String is the undesirable type
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  val success: ErrorOr[Int] = monadErrorEither.pure(42)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int]("Something wrong")

  // Similar to recover (in Try/Future)
  val handleError1: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Something wrong" => 1
    case "Something bad"   => 2
    case _                 => 0
  }

  // Similar to recoverWith
  val handleError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Something wrong" => monadErrorEither.pure(1)
    case _                 => Left("This is really bad")
  }

  // "filter"
  val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Number is too small")(_ > 3)

  // Try/Future
  import cats.instances.try_._ // implicit MonadError[Try, E]; E is always Throwable in this case
  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception)
  // technically, Try[Nothing]

  import cats.instances.future._                                       // implicit MonadError[Future, E]
  val pureFuture: Future[Int] = MonadError[Future, Throwable].raiseError(exception) // Future which will complete with Failure(exception)

  // Applicatives => ApplicativeError
  import cats.data.Validated
  type ErrorsOr[A] = Validated[List[String], A]
  import cats.ApplicativeError // Has access to the same methods as MonadError
  val applicativeErrorVal: ApplicativeError[ErrorsOr, List[String]] = ApplicativeError[ErrorsOr, List[String]]

  // Extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError(With)
  val extendedSuccess: ErrorsOr[Int] = 91.pure[ErrorsOr] // Requires ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("Something bad").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 1
  }

  import cats.syntax.monadError._ // ensure
  val testedSuccess: ErrorOr[Int] = success.ensure("Bad")(_ > 100)

  def main(args: Array[String]): Unit = {
    println(success)
    println(failure)
    println(filteredSuccess)
  }
}
