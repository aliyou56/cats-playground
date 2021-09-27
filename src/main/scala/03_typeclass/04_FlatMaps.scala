package typeclass

import cats.{ Applicative, Apply }

/**
 * Weaker Monads = Apply + flatMap method
 *
 *  Cats TC Hierarchy: 4
 *
 *    Semigroup (combine)  Functor (map)      Semigroupal (product)
 *       ^                      |__________________|
 *       |                              |
 *  Monoid (empty)                   Apply (ap)
 *                                 /          \
 *                            FlatMap       Applicative (pure)
 *                                |_______________|
 *                                      |
 *                                    Monad
 */
object FlatMaps {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(f => f(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](fa: M[A])(f: A => B): M[B] =
      flatMap(fa)(a => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ // map extension method

  def getPairs[M[_]: FlatMap, A, B](nums: M[A], chars: M[B]): M[(A, B)] =
    for {
      n <- nums
      c <- chars
    } yield (n, c)

}
