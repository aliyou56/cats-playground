package data

import cats.Semigroup

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int]    = Validated.valid(42)
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong")
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  def isPrime(n: Int): Boolean = {
    @annotation.tailrec
    def rec(d: Int): Boolean =
      if (d <= 1) true else n % d != 0 && rec(d - 1)

    if (n == 0 || n == 1 || n == -1) false else rec(Math.abs(n / 2))
  }

  // with Either
  /**
   * n must be a prime
   * n must be non-negative
   * n <= 100
   * n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String]  = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isToBig: List[String] =
      if (n <= 100) List() else List("Number must be less or equal to 100")
    val isNotPrime: List[String] = if (isPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && isPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isToBig ++ isNotPrime)
  }

  import cats.instances.list._

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated
      .cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be <= 100")))
      .combine(Validated.cond(isPrime(n), n, List("Number must be a prime")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)

  // 2. form validation
  object FormValidation {

    import cats.instances.string._

    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified"))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank"))

    def emailCheck(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid"))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(
        password.length >= 10,
        password,
        List("Password must be at least than 10 characters"),
      )

    /**
     *  fields are: name, email, password
     *  rules are:
     *  - name, email and password MUST be specified
     *  - name must not be blank
     *  - email must have @
     *  - password must have >= 10 characters
     */
    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "Name")
        .andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "Email").andThen(emailCheck))
        .combine(getValue(form, "Password").andThen(passwordCheck))
        .map(_ => "User registration complete.")
  }

  import cats.syntax.validated._ // extension methods

  val aValid: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int]      = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(validateNumber(4))

    val form = Map(
      "Name"     -> "John",
      "Email"    -> "john@company.com",
      "Password" -> "veryStrongSecret",
    )
    println(FormValidation.validateForm(form))
  }
}
