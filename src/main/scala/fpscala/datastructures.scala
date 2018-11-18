package fpscala.datastructures

import scala.annotation.tailrec

// singly-linked lists

// the + annotation on the type parameter A indicates that A is a covariant parameter of List.
// this means that if B is a subtype of A, List[B] is a subtype of List[A].
// excluding this variance annotation would restrict the scope of List[A] and exclude A's subtypes.
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

  // exercise 3.2 - works properly, but the choice to return Nil wasn't recommended by the book
  // I chose to do it because we're talking about purely functional data structures and signaling
  // an error (in the way listed) is still a side effect.
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(_, xs) => xs
    }
  }
  // exercise 3.3 - unclear if it's a good idea for the setHead function to modify empty lists
  // once again, the book did it differently than I did and signaled an error. I modified my solution
  // to be consistent with the tail fn
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => Cons(h, xs)
    }
  }

  // exercise 3.4 - is using ENSIME's type hinter cheating? (answered correctly)
  // I didn't need as much pattern matching as I used here, but it was kinda fun to match on n
  // while still easily getting drop into the correct tail position
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => n match {
        case 0 => l
        case _ => drop(t, n - 1)
      }
    }
  }

  // exercise 3.5 (answered properly)
  // I didn't get the tail-recursive implementation on the first try.
  // looking at the simpler solution given in the text's answers clued me in to the possibility
  // of putting the call to dropWhile in the tail position by applying a logical not to f in the
  // second to last case statement
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if !f(h) => l
      case Cons(_, t) => dropWhile(t, f)
    }
  }
}

object ListExamples {
  val ex1: List[Double] = Nil
  // because Nil is a case object of type Nothing (which is a subtype of all types),
  // we can consider it to be whatever type we need to
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))
}

// exercise 3.1, answered correctly
object ex3pt1 {
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  require(x == 3)
}
