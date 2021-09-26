package typeclass

/**
 * Applicative = Functors + pure
 *
 * Not quite Monads
 *    - ex: Validated
 *
 *  Cats TC Hierarchy: 2
 *
 *    Semigroup (combine)  Functor (map)      Semigroupal (product)
 *       ^                      |__________________|
 *       |                              |
 *  Monoid (empty)               Applicative (pure)
 *                                      |
 *                                    Monad
 */
object Applicatives {

  // Applicatives = Functor + the pure method
  import cats.Applicative
  import cats.instances.list._

  val listApplicative = Applicative[List]
  val aList           = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]

  val optionApplicative  = Applicative[Option]
  val anOpt: Option[Int] = optionApplicative.pure(3)

  import cats.syntax.applicative._ // pure extension method

  val aSweetList   = 2.pure[List]
  val aSweetOption = 2.pure[Option]

  // Monads extends Applicatives
  // Applicatives extend Functors
  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int]        = Validated.valid(43)    // "pure"
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 2) // map
  val validatedApplicative              = Applicative[ErrorsOr]

  // TODO: thought experiment
  def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ???

  def productWithApplicatives[W[_], A, B](
    wa: W[A],
    wb: W[B]
  )(implicit
    applicative: Applicative[W]
  ): W[(A, B)] = {

    val functionWrapper: W[B => (A, B)] =
      applicative.map(wa)(a => (b: B) => (a, b))

    applicative.ap(functionWrapper)(wb)
  }

  // Applicative have the ap method:  ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  // Applicatives can implement product from Semigroupal
  // => Applicative extends Semigroupal

  def main(args: Array[String]): Unit = {}
}
