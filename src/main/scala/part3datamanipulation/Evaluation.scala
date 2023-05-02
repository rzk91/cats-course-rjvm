package part3datamanipulation

object Evaluation {

  /*
    Cats makes a distinction between:
    - evaluating an expression eagerly (val)
    - evaluating an expression lazily and every time you request it (def)
    - evaluating an expression lazily and memoizing the value (lazy val)
   */

  import cats.Eval

  // Even an empty main will produce "Computing now!" because `instantEval` is evaluated eagerly
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    12345
  }

  // Evaluated every time we call this expression
  val alwaysEval: Eval[Int] = Eval.always {
    println("Computing each time!")
    14758
  }

  val delayedEval: Eval[Int] = Eval.later {
    println("Computing only once when called...")
    65535
  }

  val composedEval: Eval[Int] = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2

  val composedEval2: Eval[Int] = for {
    value1 <- instantEval
    value2 <- alwaysEval
  } yield value1 + value2

  // Exercise 1: What will be the output of this when this is called twice?
  val evalEx1: Eval[Int] = for {
    a <- delayedEval
    b <- alwaysEval
    c <- instantEval
    d <- alwaysEval
  } yield a + b + c + d

  // My solution: (correct!)
  /*
    Run 1:
   - Computing now!
   - Computing only once when called...
   - Computing each time!
   - Computing each time!
   - sum

   Run 2:
   - Computing each time!
   - Computing each time!
   - sum
   */

  // Make Eval remember a computed value
  val doNotRecompute: Eval[Int] = alwaysEval.memoize

  // We can chain Eval expressions and remember only what needs to be remembered in the chain
  val tutorial: Eval[String] = Eval
    .always { println("Step 1: "); "Put straps on the guitar" }
    .map { step1 =>
      println("Step 2: "); s"$step1, then put your secondary hand on the guitar neck"
    }
    .memoize // Remembering values up until this point
    .map { steps12 =>
      println("Step 3 (more complicated):")
      s"$steps12, then strike the strings with your main hand"
    }

  // Exercise 2: Implement a defer method
  /*
    Condition:
    defer(Eval.now {
      println("Now!")
      42
    })
    - This should not run any side-effects but if we call .value, it should print it
    - For others (.always or .later), it should basically do nothing
   */

  // My solution (slightly wrong: use Eval.later first!)
//  def defer[E](eval: => Eval[E]): Eval[E] = eval.flatMap(e => Eval.later(e))
  def defer[E](eval: => Eval[E]): Eval[E] = Eval.later(()).flatMap(_ => eval)

  val deferredInstantEval: Eval[Int] = defer(Eval.now { println("Computing right now!"); 42 })
  val deferredAlwaysEval: Eval[Int] = defer(alwaysEval)

  // Exercise 3: Rewrite this method with Eval
  def reverseList[A](list: List[A]): List[A] =
    if (list.isEmpty) list else reverseList(list.tail) :+ list.head

  // Solution
  def reverseListEval[A](list: List[A]): Eval[List[A]] = list match {
    case Nil          => Eval.now(list)
    case head :: tail => reverseListEval(tail).map(_ :+ head)
  }

  // But this will stack-overflow if the list is too big
  // Circumvent using defer
  // Defer (or rather Eval.later) makes everything get evaluated in a tail-recursive way
  def reverseListDefer[A](list: List[A]): Eval[List[A]] = list match {
    case Nil => Eval.now(list)
    case head :: tail => defer(reverseListDefer(tail).map(_ :+ head))
  }

  // Defer is available in the standard methods of Eval (Eval.defer)

  def main(args: Array[String]): Unit = {
    val list = (1 to 10000).toList
//    println(reverseListEval(list).value) STACK OVERFLOW!

    println(reverseListDefer(list).value) // Does NOT stack-overflow!
  }
}
