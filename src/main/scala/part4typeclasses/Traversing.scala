package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

import scala.concurrent.Future

object Traversing {

  val servers: List[String] =
    List("server-dev.example.com", "server-staging.example.com", "prod.example.com")

  def getBandwidth(hostname: String): Future[Int] =
    Future(hostname.length * 80) // Implementation not important

  val allBandwidths: Future[List[Int]] =
    servers.foldLeft(Future(List.empty[Int])) { (acc, hostname) =>
      val bw = getBandwidth(hostname)
      for {
        a <- acc
        b <- bw
      } yield a :+ b
    }

  // Seems quite complicated for a simple problem
  /*
    Problem:
    - we have a List[String]
    - We have a function String => Future[Int]
    - We need a Future[List[Int]]
   */

  // Better solution
  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // Exercise 1: Implement this method
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def listTraverse[F[_]: Monad, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (acc, a) =>
      for {
        ac <- acc
        fa <- f(a)
      } yield ac :+ fa
    }

  // But we don't need map and flatMap for this, we can use mapN from Apply and set the lower bound to Applicative
  import cats.syntax.apply._

  def listTraverseApplicative[F[_]: Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F])((acc, a) => (acc, f(a)).mapN(_ :+ _))

  // Exercise 2: Implement sequence
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverseApplicative(list)(identity)

  // Exercise 3: What do the following expressions return
  val vectorList1
    : Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4))) // Vector(List(1, 2, 3, 4))? Wrong!
  // Actually returns all tuples
  val vectorList2
    : Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // Vector(List(1, 2, 3, 4, 5, 6))? Wrong!!
  // Actually returns all triples
  // I wasn't thinking... -_-

  def filterAsOption(list: List[Int])(p: Int => Boolean): Option[List[Int]] =
    listTraverseApplicative[Option, Int, Int](list)(Option(_).filter(p))

  // Exercise 4: What is the output of these?
  val allTrueOption: Option[List[Int]] = filterAsOption(List(2, 4, 6))(_ % 2 == 0)
  // Some(List(2, 4, 6)
  val someFalseOption: Option[List[Int]] = filterAsOption(List(1, 2, 3))(_ % 2 == 0)
  // None

  import cats.data.Validated
  type ErrorsOr[A] = Validated[List[String], A]

  def filterAsValidated(list: List[Int])(p: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverseApplicative[ErrorsOr, Int, Int](list)(i =>
      Validated.cond(p(i), i, List(s"Predicate for $i failed"))
    )

  // Exercise 5: What is the output of these?
  val allTrueValidated: ErrorsOr[List[Int]] = filterAsValidated(List(2, 4, 6))(_ % 2 == 0)
  // Validated[List[String], Int].Valid(List(2, 4, 6))
  val someFalseValidated: ErrorsOr[List[Int]] = filterAsValidated(List(1, 2, 3))(_ % 2 == 0)
  // Validated[List[String], Int].Invalid(List("Predicate for 1 failed", "Predicate for 3 failed"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](coll: L[A])(f: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](coll: L[F[A]]): F[L[A]] = traverse(coll)(identity)

    // Exercise 6: Implement this method
//    type Identity[A] = A
    import cats.Id
    def map[A, B](la: L[A])(f: A => B): L[B] = traverse[Id, A, B](la)(f)
    // Because we can implement a map, we can see that Traverse extends Functor
  }

  import cats.Traverse
  val allBandwidthsCats: Future[List[Int]] = Traverse[List].traverse(servers)(getBandwidth)

  import cats.syntax.traverse._
  val allBandwidthsWithExtensions: Future[List[Int]] = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(vectorList1)
    println(vectorList2)

    printlnWithSeparator("Exercise 4")
    println(allTrueOption)
    println(someFalseOption)

    printlnWithSeparator("Exercise 5")
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
