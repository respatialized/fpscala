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
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)((x,y) => x * y)

  // exercise 3.7 - attempting to break out of the list early (answered half-correctly)

  // I think the answer is no. because product3 has a different type signature
  // than the type signature required for foldRight, you can't put recursive calls
  // to product3 inside the fold, which seems to be required for checking if any value is 0
  // and then immediately returning 0

  // my answer of no was the right answer, but I didn't really see why - foldRight has to eval
  // its argument before calling the function, which means calls to foldRight, and so on until the
  // end of the list
  def product3(ns: List[Double]): Double = {
    ns match {
      case Cons(0.0, _) => 0.0
      case _ => foldRight(ns, 1.0)((x, y) => {println(x + "," + y)
                                     if (x == 0.0 || y == 0.0) 0 else {
                                                x * y}})
    }
  }

  // exercise 3.8 - getting back the original list. (answered vaguely - the answer given in the book
  // asks you to apply foldRight stepwise on the inputs, which I didn't do)

  // if we look at the signature of apply[A] for our list below, we can see that
  // we're taking in one type of data (individual arguments),
  // transforming it into another type Cons(val, ...), and then doing it again on the smaller
  // subset of values we get from removing a (the tail) using the same data structure we're adding A to.
  // this is the same operation we perform using fold.

  val itself = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // exercise 3.9 (answered correctly)
  // I correctly grasped that it wouldn't work with (x, y) => x + 1 - this is because x is of type A
  // (which could be a string, or whatever) - y is of type B (which is an Int, the return type)
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

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
  // to be consistent with the tail fn.
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => Cons(h, xs)
    }
  }

  // exercise 3.4 - is using ENSIME's type hinter cheating? (answered correctly)
  // I didn't need as much pattern matching as I used here, but it was kinda fun to match on n
  // while still easily getting drop into the correct tail position.
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

  // exercise 3.5 (answered quasi-properly)
  // I didn't realize this was supposed to be an operation on just the list prefix, so I
  // wrote it to operate on every element of the list. read the question carefully!
  //
  // the updated signature of this function is a good intro to grouping arguments in Scala
  // functions - the type information flows "from left to right" across the argument groups.
  // seems like you can group the arguments into as many groups as you want!
  @tailrec
   def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }
  // exercise 3.6 (answered properly)
  // my first answer with four match statements seemed a bit inelegant
  // turns out that the use of tail was superfluous - you can pattern match
  // directly on the type signature to achieve the same thing.

  // this can't run in constant time because it has to iterate through every element
  // until it gets to the last Cons containing nil
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // exercise 3.10 (answered correctly)

  // this one is tail-recursive because it can evaluate f before making the recursive call to foldLeft.
  // in foldRight, the recursive call to foldRight was one of the arguments to f, which meant
  // it wasn't in the tail position.
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  // exercise 3.11 (answered correctly)
  def sum3(xs: List[Int]) = foldLeft(xs, 0)(_ + _)
  def product4(xs: List[Double]) = foldLeft(xs, 1.0)(_ * _)
  def length2[A](xs: List[A]) = foldLeft(xs, 0)((acc, _) => acc + 1)

  // exercise 3.12 (answered correctly)
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc,i) => Cons(i, acc))

  // exercise 3.13 (answered incorrectly)
  // i thought this was pretty easy once I remembered and looked at the type signatures of the curry
  // partial and compose functions from the previous chapter.

  // but then I realized reading the answer that it wouldn't work for non-commutative operations.
  // for example, using Cons as the operation reversed the list!
  // the book answer shows how you have to call reverse(as) first.

  // also, I once again didn't read through the question, because it asks you to implement foldLeft
  // via foldRight first.
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val fReversed: (B, A) => B = (b: B, a: A) => f(a, b)
    foldLeft(reverse(as), z)(fReversed)
  }

  // exercise 3.14 (answered incorrectly)
  // the flash of insight came when thinking about reverse.

  // once again, I missed that "append" here means "append a list", not "append an element."
  // I still think this answer is neat.
  def appendItem[A](as: List[A], i: A): List[A] =
    foldLeft(as, Cons(i, Nil:List[A]))((l, i) => Cons(i, l))

  // I'm still having trouble with the associativity of operations here.
  def append[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((acc, i) => Cons(i, acc))

  // foldRight works without reversing the list because it puts the entire list on the stack?
  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRight2(l, r)((i, acc) => Cons(i, acc))

  // exercise 3.15 (answered correctly)
  // I got the answer pretty quickly, but I'm not sure why. I find foldRight easier to
  // think about than foldLeft. 
  def compactLists[A](ll: List[List[A]]): List[A] = {
    foldRight2(ll, Nil:List[A])((i, acc) => append2(i, acc))
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
