package data

object Evaluation {

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    1233
  }

  val redoEval: Eval[Int] = Eval.always {
    println("Computing again!")
    321
  }

  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later!")
    741
  }

  val composedEvaluation: Eval[Int] =
    instantEval.flatMap(v1 => delayedEval.map(v2 => v1 + v2))

  val evalEx1: Eval[Int] = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  val dontRecompute = redoEval.memoize

  val ev = Eval
    .always { println("Step 1"); "put the guitar on your lap" }
    .map { step1 => println("step 2"); s"$step1 then put your left hand on the neck" }
    .memoize
    .map { step12 =>
      println("step 3, more complicated"); s"$step12 then with the right hand strike the strings"
    }

  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty)
      Eval.now(list)
    else
      defer(reverseEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    // println(composedEvaluation.value)
    // println(evalEx1.value)
    // println(evalEx1.value)

    defer(Eval.now {
      println("Now")
      42
    }).value

    println(reverseEval((1 to 10000).toList).value)
  }
}
