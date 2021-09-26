package typeclass

import java.util.concurrent.Executors

import scala.concurrent.{ ExecutionContext, Future }

import cats.{ Functor, Semigroupal }

/**
 * Weaker Applicatives = Functor + Semigroupal + ap method
 * Convenient for extracting and combining tuples
 *
 *  Cats TC Hierarchy: 3
 *
 *    Semigroup (combine)  Functor (map)      Semigroupal (product)
 *       ^                      |__________________|
 *       |                              |
 *  Monoid (empty)                   Apply (ap)
 *                                      |
 *                               Applicative (pure)
 *                                      |
 *                                    Monad
 */
object Applies {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    def ap[A, B](wf: W[A => B])(wa: W[A]): W[B] = ??? // fundamental

    // TODO
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2) // W[(A, B)]
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }
    }

    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental method
  }

  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]

  val applyOption = Apply[Option]
  val funcApp     = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._ // extension method from Apply
  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple  = tupleOfOptions.tupled          // Some((1, 2, 3))
  val sumOption      = tupleOfOptions.mapN(_ + _ + _) // Some(6)

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val tupleOfFutures = (Future(1), Future(2), Future(42))
  val futureOfTuple  = tupleOfFutures.tupled

  def main(args: Array[String]): Unit =
    futureOfTuple.foreach(println)
}
