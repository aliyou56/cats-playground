package abstraction

import java.util.concurrent.Executors

import scala.concurrent.{ ExecutionContext, Future }

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT     // option transformer
  import cats.instances.list._ // fetch an implicit Monad[List]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))

  val listOfCharOptions: OptionT[List, Char] = OptionT(
    List(Option('a'), Option('b'), Option.empty[Char])
  )

  val listOfTuples: OptionT[List, (Int, Char)] =
    for {
      number <- listOfNumberOptions
      char   <- listOfCharOptions
    } yield (number, char)

  import cats.data.EitherT // either transformer
  val listOfEither: EitherT[List, String, Int] = EitherT(List(Left("Boom"), Right(43), Right(2)))

  import cats.instances.future._
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  /**
   * Exercise
   */

  val bandwidths = Map(
    "server1.com" -> 50,
    "server2.com" -> 300,
    "server3.com" -> 170,
  )
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandWidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match {
      case None    => EitherT.left(Future(s"Server $server unreachable"))
      case Some(b) => EitherT.right(Future(b))
    }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      b1 <- getBandWidth(s1)
      b2 <- getBandWidth(s2)
    } yield b1 + b2 > 250

  def generateTrafficReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 cannot cope with the incoming spike: $reason")
      case Right(false) =>
        Left(s"Servers $s1 and $s2 cannot cope with the incoming spike: not enough total bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the incoming spike Yey :)")
    }

  def main(args: Array[String]): Unit =
//    println(listOfTuples.value)
    generateTrafficReport("server1.com", "server2.com").value.foreach(println)
}
