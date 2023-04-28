package part2abstractMath

import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val listMonad: Monad[List] = Monad[List]

  // Either is also a Monad
  val either: Either[String, Int] = Right(5)
  // How Either is used normally
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val eitherLoading: LoadingOr[Int] = loadingMonad.pure(45)

  val eitherLoadingChanged: LoadingOr[Int] = loadingMonad.flatMap(eitherLoading)(n =>
    if (n % 2 == 0) Right(n + 1) else Left("Not correct...")
  )

  // Imaginary online store
  case class OrderStatus(id: Long, status: String)

  // Some API (implementation not important)
  def getOrderStatus(id: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(id, "Ready"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.id > 1000) Left("Not available yet") else Right("On its way!")

  val orderId: Long = 1234

  val orderLocation: LoadingOr[String] =
    loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  // With extension methods (actually not needed because Either already has map/flatMap
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)

  val orderLocationFor: LoadingOr[String] = for {
    status   <- getOrderStatus(orderId)
    location <- trackLocation(status)
  } yield location

  // Exercise 1: Service level API for web app
  case class Connection(host: String, port: String)
  val config: Map[String, String] = Map("host" -> "localhost", "port" -> "4040")

  trait HttpService[M[_]] {
    def getConnection(conf: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  // Generic get response method
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit M: Monad[M]): M[String] =
    for {
      conn     <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  // Requirements:
  //  - If host and port are found -> return M[Connection]
  //  - If there is a failure -> return appropriate return value for M[_] (e.g. Failure for Try, None for Option, ...)
  //  - If issueRequest has payload less than 20 characters, return "request $payload has been accepted", else fail
  // Compose a real implementation of HttpService using at least one of Try, Option, Either, Future

  // Solution:
  object TryForOption {

    implicit class TryOps[A](private val option: Option[A]) extends AnyVal {
      def toTry: Try[A] = Try(option.get)
    }
  }

  object TryHttpService extends HttpService[Try] {
    import TryForOption._

    def getConnection(conf: Map[String, String]): Try[Connection] =
      for {
        host <- conf.get("host").toTry
        port <- conf.get("port").toTry
      } yield Connection(host, port)

    def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.lengthCompare(20) < 0) {
        Success(s"Request [$payload] has been accepted")
      } else {
        Failure(new IllegalArgumentException("Invalid payload"))
      }
  }

  object HttpServiceWithError extends HttpService[ErrorOr] {

    def getConnection(conf: Map[String, String]): ErrorOr[Connection] =
      for {
        host <- conf.get("host").toRight(new RuntimeException("Host missing"))
        port <- conf.get("port").toRight(new RuntimeException("Port missing"))
      } yield Connection(host, port)

    def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.lengthCompare(20) < 0) {
        Right(s"Request [$payload] has been accepted")
      } else {
        Left(new RuntimeException("Payload too long"))
      }
  }

  def main(args: Array[String]): Unit = {
    println(either)
    println(eitherLoading)
    println(eitherLoadingChanged)

    println(orderLocation)

    val responseTry = TryHttpService
      .getConnection(config)
      .flatMap(TryHttpService.issueRequest(_, "Hello, Http service!"))

    val responseTryFor = for {
      conn     <- TryHttpService.getConnection(config)
      response <- TryHttpService.issueRequest(conn, "Hello, Http service, how ya doin'!")
    } yield response

    println(responseTry)
    println(responseTryFor)

    val responseError = for {
      conn     <- HttpServiceWithError.getConnection(config - "host")
      response <- HttpServiceWithError.issueRequest(conn, "What's up!")
    } yield response

    println(responseError)

    // Using generic getResponse method
    println("\n====== Using generic getResponse method ======")
    println(getResponse(TryHttpService, "Hello, What's up!"))
    println(getResponse(HttpServiceWithError, "Bla bla bla bla bla bla bla"))
  }
}
