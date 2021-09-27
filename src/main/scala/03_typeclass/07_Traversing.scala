package typeclass

import java.util.concurrent.Executors

import scala.concurrent.{ ExecutionContext, Future }

import cats.{ Applicative, Foldable, Functor }

/**
 * Higher-kinded TC with "inside-out" functions
 *
 *     Semigroup (combine)     Foldable       Functor (map)      Semigroupal (product)
 *          ^                      |____________|   |__________________|
 *          |                            |                   |
 *     Monoid (empty)                Traverse            Apply (ap)
 *                                                      /          \
 *                                                 FlatMap       Applicative (pure)
 *                                                     |_______________|      \
 *                                                           |                 \
 *                                                         Monad       ApplicativeError (raiseError, handleError.With)
 *                                                             \_______________/
 *                                                                     |
 *                                                                 MonadError
 */
object Traversing {

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers: List[String] = List("server-ci.rtjvm.com", "staging.rtjvm.com", "prod.rtjvm.com")

  def getBanWidth(hostname: String): Future[Int] = Future(hostname.length * 15)

  val allBandWidths: Future[List[Int]] =
    servers.foldLeft(Future(List.empty[Int])) { (acc, hostname) =>
      for {
        accBandwidths <- acc
        band          <- getBanWidth(hostname)
      } yield accBandwidths :+ band
    }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBanWidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBanWidth))

  // TODO 1
  import cats.syntax.applicative._ // pure
  import cats.syntax.apply._       // mapN

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wrapperAcc, a) =>
      (wrapperAcc, f(a)).mapN(_ :+ _)
    }
//  list.foldLeft(implicitly[Applicative[F]].pure(List.empty[B])) { (wrapperAcc, a) =>
//    implicitly[Applicative[F]].map2(wrapperAcc, f(a))(_ :+ _)
//  }

  // TODO 2
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  import cats.instances.vector._

  val allPairs: Vector[List[Int]] = listSequence(
    List(Vector(1, 2), Vector(3, 4))
  ) // all the possible 2-pairs
  val allTriples: Vector[List[Int]] = listSequence(
    List(Vector(1, 2), Vector(3, 4), Vector(5, 6))
  ) // all the possible 3-pairs

  // Option
  import cats.instances.option._

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))

  val allTrue: Option[List[Int]]   = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2,4,6))
  val someFalse: Option[List[Int]] = filterAsOption(List(1, 4, 3))(_ % 2 == 0) // None

  // Validated
  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] => Applicative[ErrorsOr]

  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  val allTrueValidated: ErrorsOr[List[Int]] =
    filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2,4,6))
  val someFalseValidated: ErrorsOr[List[Int]] =
    filterAsValidated(List(1, 4, 3))(_ % 2 == 0) // Invalid(List(...))

  trait MyTraverse[CC[_]] extends Foldable[CC] with Functor[CC] {

    def traverse[F[_]: Applicative, A, B](container: CC[A])(f: A => F[B]): F[CC[B]]

    def sequence[F[_]: Applicative, A](container: CC[F[A]]): F[CC[A]] =
      traverse(container)(identity)

    import cats.Id

    def map[A, B](wa: CC[A])(f: A => B): CC[B] =
      traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]

  val allBandwidthCats = Traverse[List].traverse(servers)(getBanWidth)

  import cats.syntax.traverse._ // extension methods: sequence + traverse

  val allBandwidthsCats2 = servers.traverse(getBanWidth)

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
