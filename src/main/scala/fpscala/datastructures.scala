package fpscala.datastructures

// singly-linked lists

// the + annotation on the type parameter A indicates that A is a covariant parameter of List.
// this means that if B is a subtype of A, List[B] is a subtype of List[A].
// excluding this variance annotation would restrict the scope of List[A] and exclude A's subtypes
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // here's our familiar recursive list-consuming function template from sicp/htdp,
  // this time with strong static types
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}

object ListExamples {
  val ex1: List[Double] = Nil
  // because Nil is a case object of type Nothing (which is a subtype of all types),
  // we can consider it to be whatever type we need to
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))
}
