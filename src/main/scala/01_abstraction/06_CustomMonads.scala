package abstraction

import cats.Monad

object CustomMonads extends App {

  implicit object OptionMonad extends Monad[Option] {

    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    @annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None           => None
        case Some(Left(a))  => tailRecM(a)(f)
        case Some(Right(b)) => Some(b)
      }
  }

  type Identity[T] = T

  implicit object IdentityMonad extends Monad[Identity] {

    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

    @annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] =
      f(a) match {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => b
      }
  }

  val aNumber: Identity[Int] = 42

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A)                        extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(value)         => f(value)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v))       => stackRec(f(v))
        case Leaf(Right(b))      => Leaf(b)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

      @annotation.tailrec
      def tailRec(
          todo: List[Tree[Either[A, B]]],
          expanded: Set[Tree[Either[A, B]]],
          done: List[Tree[B]],
        ): Tree[B] =
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(Left(v))  => tailRec(f(v) :: todo.tail, expanded, done)
            case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
            case node @ Branch(left, right) =>
              if (!expanded.contains(node))
                tailRec(left :: right :: todo, expanded + node, done)
              else {
                val newLeft   = done.head
                val newRight  = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(todo.tail, expanded, newBranch :: done.drop(2))
              }
          }

//      stackRec(f(a))
      tailRec(List(f(a)), Set(), List())
    }
  }
}
