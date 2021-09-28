package typeclass

/**
 * Used for:
 * - function composition when they return F[_] types
 * - Dependency injection - Reader!
 */
object Kleislis {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int]    = x => Some(x * 3)

  // val func3 = func2 andThen func1

  val plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else "not even"
  val plainFunc2: Int => Int    = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  import cats.data.Kleisli
  import cats.instances.option._ // implicit FlatMap[Option]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int]    = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multiply: Kleisli[Option, Int, Int] = func2K.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain: Kleisli[Option, Int, String] = func2K.flatMap(x => func1K)

  // TODO
  import cats.Id

  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // InterestingKleisli === Reader!

  val times2: Kleisli[Id, Int, Int]   = Kleisli[Id, Int, Int](x => x * 2)
  val plus4: Kleisli[Id, Int, Int]    = Kleisli[Id, Int, Int](y => y + 4)
  val composed: Kleisli[Id, Int, Int] = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor: Kleisli[Id, Int, Int] = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  def main(args: Array[String]): Unit = {
    println(func3K(2))
    println(composedFor(3))
  }
}
