package data

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(curr => (curr, s"Counted $curr"))
  val (eleven, counted10)             = countAndSay.run(10).value
  // state = "iterative" computations

  var a                = 10
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  val firstTransformation: State[Int, String] =
    State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))

  val secondTransformation: State[Int, String] =
    State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))

  val compositeTransformation: State[Int, (String, String)] =
    for {
      res1 <- firstTransformation
      res2 <- secondTransformation
    } yield (res1, res2)

  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")
  val compositeFunc = func1 andThen {
    case (newState, res1) => (res1, func2(newState))
  }

  // an online Store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State { cart =>
      (ShoppingCart(item :: cart.items, cart.total + price), price + cart.total)
    }

  val aCart: State[ShoppingCart, Double] =
    for {
      _     <- addToCart("Laptop", 1000)
      _     <- addToCart("Smartphone", 300)
      total <- addToCart("car", 10000)
    } yield total

  // returns s State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State { a =>
    (a, f(a))
  }

  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State { a =>
    (a, a)
  }

  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State { _ =>
    (value, ())
  }

  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State { a =>
    (f(a), ())
  }

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 10)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(compositeFunc(10))
    println(aCart.run(ShoppingCart(List(), 0)).value)
    println(program.run(10).value)
  }
}
