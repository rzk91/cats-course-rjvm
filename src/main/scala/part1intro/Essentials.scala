package part1intro

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Essentials {

  val executorService: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(executorService)

  val futureVal: Future[Int] = Future {
    // Something asynchronous
    91
  }

  futureVal.onComplete {
    case Success(value)     => println(s"Successful value: $value")
    case Failure(exception) => println(s"Value failed: $exception")
  }

  futureVal.map(_ * 2)

  def main(args: Array[String]): Unit = {
    for {
      v <- futureVal
    } yield println(s"Value is $v")

    executorService.shutdown()
  }
}
