package typeclass

import cats.Eval
import cats.kernel.Monoid

/**
 *  Cats TC Hierarchy: 6
 *
 *     Semigroup (combine)     Foldable       Functor (map)      Semigroupal (product)
 *          ^                                        |__________________|
 *          |                                                |
 *     Monoid (empty)                                     Apply (ap)
 *                                                      /          \
 *                                                 FlatMap       Applicative (pure)
 *                                                     |_______________|      \
 *                                                           |                 \
 *                                                         Monad       ApplicativeError (raiseError, handleError.With)
 *                                                             \_______________/
 *                                                                     |
 *                                                                 MonadError
 */
object Folding {

  // TODO - implement all in terms of foldLeft & foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B]) { (a, currentList) =>
        f(a) :: currentList
      }
//      list.foldLeft(List.empty[B]) { (currentList, a) => f(a) :: currentList }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
//      list.foldLeft(List.empty[B]) { (currentList, a) => currentList ++ f(a) }
      list.foldLeft(List.empty[B]) { (currentList, a) =>
        currentList.foldRight(f(a))(_ :: _)
      }

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A]) { (a, currentList) =>
        if (predicate(a)) a :: currentList else currentList
      }

    def combineAll[A: Monoid](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldRight(monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]

  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._ // implicit Foldable[Option]

  val sumOpt = Foldable[Option].foldLeft(Option(2), 5)(_ + _) // 7

  // foldRight is stack-safe regardless of your container
  val sumRight: Eval[Int] =
    Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
      eval.map(_ + num)
    }

  import cats.instances.int._

  val anotherSum: Int = Foldable[List].combineAll(List(1, 2, 3)) // requires implicit Monoid[Int]

  import cats.instances.string._

  val mappedConcat: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // "123"

  import cats.instances.vector._

  val intsNested         = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val combineNested: Int = (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  import cats.syntax.foldable._ // extension methods

  val sum3          = List(1, 2, 3).combineAll // req Foldable[List], Monoid[Int]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {

    import ListExercises._

    val numbers = (1 to 10).toList
    println(map(numbers)(_ + 1))
    println(flatMap(numbers)(x => List(x + 1)))
    println(filter(numbers)(_ % 2 == 0))

    import cats.instances.int._ // Monoid[Int]

    println(combineAll(numbers))

    println(sum)
    println(sumOpt)
    println(anotherSum)
    println(mappedConcat)
    println(combineNested)
  }
}
