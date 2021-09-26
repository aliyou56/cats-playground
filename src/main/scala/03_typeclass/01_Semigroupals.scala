package typeclass

import java.util.concurrent.Executors

import scala.concurrent.{ ExecutionContext, Future }

import cats.Monad

/**
 * Higher-kinded type class which can tuple elements
 * Monads extends Semigroupals
 *  - product is implemented in terms of map/flatMap
 *
 * Some Semigroupals are useful without being monads
 *  - exemple: Validated
 *
 *  Semigroup vs Semigroupal: (combining vs tupling)
 *
 *  Cats TC Hierarchy: 1
 *
 *    Semigroup              Functor       Semigroupal
 *       ^                      |______________|
 *       |                              |
 *     Monoid                          Monad
 */
object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]

  val optSemigroupal = Semigroupal[Option]
  val aTupledOpt: Option[(Int, String)] =
    optSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled: Option[(Int, Nothing)] = optSemigroupal.product(Some(123), None) // None

  import cats.instances.future._

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val aTupledFuture: Future[(String, Int)] =
    Semigroupal[Future].product(Future("the meaning of life"), Future(42))

  import cats.instances.list._ // Monad[List]

  val aTupledList: List[(Int, String)] = Semigroupal[List].product(List(1, 2), List("a", "b"))

  // TODO: implement product with monads
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def productWithMonads[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // Monads extends Semigroupals

  trait MyMonad[M[_]] extends MySemigroupal[M] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))

    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))
  }

  // example use case of Semigroupals: Validated
  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]

  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]
  val invalidCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  // Monadic type
  type EitherErrorsOr[T] = Either[List[String], T]

  import cats.instances.either._ // implicit Monad[Either] / Either is a Monad

  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // implemented in terms of map & flatMap
    Left(
      List("Something wrong", "something else wrong")
    ),                                // flatMap shortcut the evaluation the 2sd either
    Left(List("This can't be right")) // not propagated because the first one is a Left
  )

  // associativity law:
  // m.flatMap(f).flatMap(g) == m.flatMap(w => f(x).flatMap(g))
  // true for either - Not true for Validated

  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] = listA.zip(listB)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)
    println(productWithMonads(List(1, 2), List("a", "b")))
    println(invalidCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(List(1, 2), List("a", "b")))
  }
}
