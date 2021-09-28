package typeclass

import cats.kernel.Monoid

/**
 *                                           Invariant <---------- Contravariant
 *                                               ^
 *                                               |
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
object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String     = self.encrypt(back(value))
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A  = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String     = value.map(c => (c + 2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  /**
   *  How can we support ints, doubles, Option[String] ?
   */
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  // TODO
  implicit val optionStringCrypto: Crypto[Option[String]] =
    caesarCypher.imap(_.getOrElse(""), Option.apply)

  // TODO 2
  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option.apply)

  import cats.Invariant
  import cats.Show
  import cats.instances.string._ // Show[String]

  val showString = Show[String]
  val showOptionString: Show[Option[String]] =
    Invariant[Show].imap(showString)(Option.apply)(_.getOrElse(""))

  import cats.syntax.invariant._

  val showOptionString2: Show[Option[String]] = showString.imap(Option.apply)(_.getOrElse(""))

  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[_])(back: B => A): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // covariant functor
    def map[A, B](wa: W[A])(forth: A => B): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      map(wa)(forth)
  }

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)

    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))

    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Math.PI)))
  }
}
