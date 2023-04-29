package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = values.flatten.sum

  // Option Transformer
  import cats.data.OptionT
  import cats.instances.list._
  // fetches an implicit OptionT[List], which is basically a wrapper for List[Option[_]]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2), None))

  val listOfCharOptions: OptionT[List, Char] = OptionT(
    List(Option('a'), Option('b'), Option.empty[Char])
  )

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char   <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // So, OptionT offers API to perform operations on a monad of monads without having to unwrap the inner monad

  // Either Transformer
  import cats.data.EitherT
  import cats.instances.future._
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Right(1), Left("Unknown"), Right(3)))

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  // Exercise 1
  // There is going to be a traffic spike because of some media happening. Bandwidth is measured in some units.
  // Allocate 2 (TWO) servers to cope with the traffic surge
  // Current capacity of each server is known; Traffic can be supported if sum of bandwidths > 250
  val bandwidths: Map[String, Int] = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None        => EitherT.left(Future(s"Server $server unreachable"))
    case Some(value) => EitherT.right(Future(value))
  }

  // Solution
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      b1 <- getBandwidth(s1)
      b2 <- getBandwidth(s2)
    } yield b1 + b2 > 250

  def generateTrafficSurgeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"$s1 and $s2 cannot cope with expected surge: $reason")
      case Right(false) => Left(s"$s1 and $s2 do not have enough bandwidth for surge")
      case _            => Right(s"$s1 and $s2 can withstand surge")
    }

  // Exercise 2 (my own): Implement sumAllOptions using OptionT
  def sumAllOptionsT(values: List[Option[Int]]): Int = OptionT(values).foldLeft(0)(_ + _)

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    // generateTrafficSurgeReport("server2", "server3").value.foreach(println)

    println(sumAllOptions(listOfNumberOptions.value))
    println(sumAllOptionsT(listOfNumberOptions.value))
  }
}
