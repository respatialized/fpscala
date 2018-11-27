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

  // exercise 3.16 (answered correctly)
  //
  // I got the right answer, but the answer given in the book uses fold, which definitely is
  // cleaner (if not quite as explicit in its logic as my version)
  // also, my version isn't tail-recursive.
  def addOne(l: List[Int]): List[Int] = {
    l match {
      case Cons(n, Nil) => Cons(n + 1, Nil)
      case Cons(n, ls) => Cons(n + 1, addOne(ls))
      case Nil => Nil
    }
  }

  // exercise 3.17 (answered correctly)
  // it's hard to remember the syntax of foldRight! maybe I should make a flashcard.
  // I also didn't use a stack-safe version of foldRight at first.
  def doubleToString(l: List[Double]): List[String] = {
    foldRight2(l, Nil: List[String])((i, acc) => Cons(i.toString, acc))
  }

  // exercise 3.18 (answered correctly)
  // this book really puts an emphasis on building up from concrete cases to abstractions,
  // both in the chapter sequence and in the sequence of exercises.
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight2(as, Nil: List[B])((a, acc) => Cons(f(a), acc))
  }

  // exercise 3.19 (answered correctly, but not stack safe)
  // @tailrec
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
      case Cons(_, t) => filter(t)(f)
    }
  }

  // can I do it using fold? or in a stack-safe way?
  // I don't know how.

  // it can be done using fold - I just forgot about the fact that using fold and Cons
  // returns the list itself - cons + fold is isomorphic with the regular list structure
  // it works because the return type of the fold is  the same as the input type.
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight2(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  // exercise 3.20 (answered correctly)
  // I used foldRight and the book used map, but it's the same idea.
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight2(as, Nil: List[B])((a, acc) => append2(f(a), acc))
  }

  // exercise 3.21 (works as expected) (answered correctly)
  // the only difference was that I added a type annotation to Nil.
  def filter3[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)({(a) =>
                  if (f(a)) List(a) else Nil: List[A]
                })
  }

  // exercise 3.22 (works as expected)
  // once again, the book has a less verbose solution.
  def addLists(xs: List[Int], ys: List[Int]): List[Int] = {
    (xs, ys) match {
      case (xs, ys) if (length(xs) != length(ys)) => sys.error("mismatched list sizes")
      case (Nil, Nil) => sys.error("empty lists")
      case (Cons(x, Nil), Cons(y, Nil)) => Cons(x + y, Nil)
      case (Cons(x, xt), Cons(y, yt)) => Cons(x + y, addLists(xt, yt))
    }
  }

  // can I do it in a stack-safe way? the book's answer suggests: no.

  // exercise 3.23 (works as expected)
  // the book's solution was more general because it took three type parameters.
  // mine assumed that the input lists were of the same type when that's not necessary to
  // solve the problem. In general, one benefit of a type system is that it allows you
  // to solve the problem with the fewest number of assumptions by abstracting them away.

  // remember: even if you add three type annotations, they all can be the same type when the
  // function is called.
  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] = {
    (xs, ys) match {
      case (xs, ys) if (length(xs) != length(ys)) => sys.error("mismatched list sizes")
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xt), Cons(y, yt)) => Cons(f(x, y), zipWith(xt, yt)(f))
    }
  }

  // exercise 3.24 (works as expected)
  // damn, the ghost of a problem from 4Clojure has come back to haunt me in Scala!
  // the solution came to me when I realized that for this problem I only need
  // the subseqs that are the same size as the smaller list

  // the book's solution was better because it did a letter by letter comparison,
  // pattern matching on the heads of both lists and recursively calling on the tails.
  // this allowed a stack-safe implementation, which mine definitely is not.
  def hasSubsequence[A](big: List[A], small: List[A]): Boolean = {

    val subseqs = getSubSeqs(big, length2(small))

    def go[A](l: List[A], small: List[A]): Boolean = {
      l match {
        case Cons(h, _) if h == small => true
        case Cons(_, Nil) => false
        case Cons(h, t) => go(t, small)
      }
    }
    go(subseqs, small)
  }

  def getSubSeqs[A](l: List[A], size: Int): List[List[A]] = {
    def getFirstSubseq[A](l: List[A], size: Int): List[A] = {
      reverse(drop(reverse(l), length(l) - size))
    }
    l match {
      case Nil => Nil
      case Cons(i, Nil) if size == 1 => List(List(i))
      case Cons(i, Nil) => Nil
      case Cons(h, t) => Cons(getFirstSubseq(l, size), getSubSeqs(t, size))
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


sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {

  def apply[A](v: A): Tree[A] = new Leaf[A](v)
  def apply[A](l: Tree[A], r: Tree[A]): Tree[A] = new Branch[A](l, r)

  // exercise 3.25 (works as expected) (answered correctly)

  // can this be done in a tail-recursive way?
  // the book suggests no.
  def count[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + count(l) + count(r)
    }
  }

  // exercise 3.26 (works as expected) (answered correctly)
  // the compiler warning about an unhandled case gave me the idea of simplifying the match expression.
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l).max(maximum(r))
    }
  }

  // exercise 3.27 (works as expected) (answered correctly)
  // I found this solution to be particularly elegant.

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + depth(l).max(depth(r))
    }
  }

  // exercise 3.28 (works as expected) (answered correctly)
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Tree(f(v))
      case Branch(l, r) => Tree(map(l)(f), map(r)(f))
    }
  }

  // exercise 3.29 (works as expected) (answered correctly)
  // this was a really good challenge. I don't know exactly where I got the idea to
  // break it out into two functions (apart from just focusing hard on the type signatures),
  // but it certainly helped!

  // definitely proud of myself for this one.
  def fold[A, B]
    (t: Tree[A])
          (fa: A => B) // atomic function (for leaves)
          (fc: (B, B) => B) // comparing/combining function (for branches)
      : B = {
    t match {
      case Leaf(v) => fa(v)
      case Branch(l, r) => fc(fold(l)(fa)(fc), fold(r)(fa)(fc))
    }
  }

  def countViaFold[A](t: Tree[A]) = fold(t)(x => 1)(_ + _)
  def maximumViaFold(t: Tree[Int]) = fold(t)(x => x)((x, y) => x.max(y))
  def depthViaFold[A](t: Tree[A]) = fold(t)(x => 0)((x, y) => 1 + x.max(y))
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Tree(f(x)))((x, y) => Tree(x, y))
}
