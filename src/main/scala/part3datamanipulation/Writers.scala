package part3datamanipulation

import cats.{Functor, Monoid}
import cats.data.WriterT

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.Try

object Writers {

  // Writers help in keeping track of important information in an application while data is manipulated
  // E.g. loggers? I was right!

  import cats.data.Writer
  val someWriter: Writer[List[String], Int] = Writer(List("Started something"), 91)

  // Value increased, logs remain the same
  val increasedWriter: Writer[List[String], Int] = someWriter.map(_ + 1)

  // Value stays the same, logs change
  val logsWriter: Writer[List[String], Int] =
    someWriter.mapWritten(_ :+ "Found something interesting")

  // Both value and logs changed (but independently)
  val bothWriter: Writer[List[String], Int] =
    someWriter.bimap(_ :+ "found something interesting", _ + 1)

  // Both value and logs changed (but value also used in logs; so, dependent)
  val bothWriter2: Writer[List[String], Int] = someWriter.mapBoth { (logs, value) =>
    (logs :+ s"found something interesting: $value", value + 1)
  }

  /*
  Pattern
  - Define a writer at the start
  - Manipulate them with pure FP
  - Dump either value or logs or both
   */

  // Dumping them
  val desiredValue: Int = someWriter.value
  val desiredLogs: List[String] = someWriter.written
  val (logs, value) = someWriter.run

  // Different scenario -- flatMap
  val writerA: Writer[Vector[String], Int] = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB: Writer[Vector[String], Int] = Writer(Vector("Log B1"), 50)

  val compositeWriter: Writer[Vector[String], Int] = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // Reset the logs
  val emptyWriter: Writer[List[String], Int] = someWriter.reset

  // Exercise 1: Rewrite a function that "prints" things with Writer
  def countAndSay(n: Int): Unit =
    if (n <= 0) println("Starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }

  // Solution
  def countAndLogTailrec(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def loop(m: Int, writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] =
      if (m == n) writer.mapWritten(_ :+ s"$n")
      else loop(m + 1, writer.mapWritten(_ :+ s"$m"))

    val emptyWriter = Writer(Vector("Starting!"), 0)
    if (n <= 0) emptyWriter else loop(1, emptyWriter)
  }

  // Solution (not tail-recursive)
  def countAndLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector("Starting!"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))

  // Exercise 2: Rewrite this method with Writers
  def naiveSum(n: Int): Int =
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }

  // Solution: Can be improved... (And not exactly the same thing)
  def sumWithLogsTailrec(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def loop(m: Int, acc: Writer[Vector[String], Int]): Writer[Vector[String], Int] =
      if (m == n) acc.map(_ + n)
      else {
        loop(
          m + 1,
          acc.mapBoth { (logs, prevSum) =>
            val newSum = prevSum + m
            (logs ++ Vector(s"Computed sum($m) = $newSum", s"Now at ${m + 1}"), prevSum + m)
          }
        )
      }

    val emptyWriter = Writer(Vector.empty[String], 0)
    if (n <= 0) emptyWriter
    else loop(1, emptyWriter.mapWritten(_ => Vector("Computed sum(0) = 0", "Now at 1")))
  }

  // Better looking (and more exact) solution (but not tail-recursive)
  def sumWithLogs(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector.empty[String], 0)
    else
      for {
        _        <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _        <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n

  def _main(args: Array[String]): Unit = {
    println(desiredValue)
    println(desiredLogs)
    println(bothWriter.run)
    println(bothWriter2.run)

    println(compositeWriter.run)

    println(emptyWriter.run)

    println("\n ===== Exercise 1 ======")
    println(countAndSay(5))
    countAndLogTailrec(5).written.foreach(println)
    countAndLog(5).written.foreach(println)

    println("\n ===== Exercise 2 =====")
    println(naiveSum(5))
    println(sumWithLogsTailrec(5).value)
    sumWithLogsTailrec(5).written.foreach(println)
    sumWithLogs(5).written.foreach(println)

    println("\n ===== Why naive sum is bad =====")
    Future(naiveSum(10)).foreach(println)
    Future(naiveSum(10)).foreach(println)

    println("\n ===== using Future with Writers =====")
    val sumFuture1 = Future(sumWithLogsTailrec(5))
    val sumFuture2 = Future(sumWithLogsTailrec(5))
    val logs1 = sumFuture1.map(_.written) // Logs from thread 1
    val logs2 = sumFuture2.map(_.written) // Logs from thread 2
    logs1.foreach(_.foreach(println))
    logs2.foreach(_.foreach(println))
  }

  // Why does WriterT's liftF method use an Applicative instead of just a Functor? We just need the `map` method...
  def liftF_[F[_], L, V](fv: F[V])(implicit monoidL: Monoid[L], F: Functor[F]): WriterT[F, L, V] =
    WriterT(F.map(fv)(v => (monoidL.empty, v)))

  def main(args: Array[String]): Unit = {
    val writer = someWriter.tell(List("Something else")).tell(List("And something more"))
    writer.written.foreach(println)

    val writer2 = WriterT.liftF[Option, List[String], Int](Option(2))
    writer2.tell(List("Lifting Option(2)")).run.foreach(println)

    liftF_[Try, Vector[String], Int](Try(3)).tell(Vector("Seems to work just fine!")).run.foreach(println)
  }
}
