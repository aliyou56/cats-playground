package data

import java.util.concurrent.Executors

import scala.concurrent.{ ExecutionContext, Future }

object Writers {

  import cats.data.Writer

  // 1. define at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 42)

  // 2. manipulate them with pure FP
  val anIncreaseWriter = aWriter.map(_ + 1)
  val aLogsWriter      = aWriter.mapWritten(_ :+ "found something interesting")
  val aWriterWithBoth  = aWriter.bimap(_ :+ "found something coll", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something", value + 1)
  }

  // 3. dump either the value or the logs
  val desiredValue = aWriter.value
  val logs         = aWriter.written
  val (l, v)       = aWriter.run

  import cats.instances.vector._ // imports a Semigroup[Vector]
  val writerA: Writer[Vector[String], Int] = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB: Writer[Vector[String], Int] = Writer(Vector("Log B1", "Log B2"), 40)
  val compositeWriter: Writer[Vector[String], Int] =
    for {
      va <- writerA
      vb <- writerB
    } yield va + vb

  // reset the logs
  import cats.instances.list._ // an implicit Monoid[List[Int]]
  val anEmptyWriter: Writer[List[String], Int] = aWriter.reset // clear the logs, keep the value

  // function which "prints" things with writers
  def countAndSay(n: Int): Unit =
    if (n <= 0)
      println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }

  def countAndSayLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0)
      Writer(Vector("Starting!"), 0)
    else
      countAndSayLog(n - 1)
        .flatMap(_ => Writer(Vector(s"$n"), n))

  def naiveSum(n: Int): Int =
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }

  def sumLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0)
      Writer(Vector(), 0)
    else
      for {
        -        <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumLog(n - 1)
        -        <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService((Executors.newFixedThreadPool(8)))

  def main(args: Array[String]): Unit = {
    // println(compositeWriter.run)
    // countAndSayLog(10).written.foreach(println)

    // println(naiveSum(10))
    // sumLog(10).written.foreach(println)

    // Future(naiveSum(100)).foreach(println)
    // Future(naiveSum(100)).foreach(println)

    // Benefit: Writers can keep logs separate on multiple threads
    Future(sumLog(5)).map(_.written).foreach(println) // f1
    Future(sumLog(5)).map(_.written).foreach(println) // f2
  }
}
