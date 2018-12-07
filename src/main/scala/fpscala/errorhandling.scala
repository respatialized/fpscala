package fpscala.errorhandling

import scala.{Option => _, Either => _, _}

object ExceptionExamples {


  def meanBad(xs: Seq[Double]): Double = {
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list")
    else xs.sum/xs.length
  }

  sealed trait Option[+A] {
    // exercise 4.1 (answered incorrectly)
    // everything except map and getOrElse can be done without pattern matching,
    // according to the book

    // I forgot about the this keyword.
    // I really wasn't able to get this one in the way the textbook describes.
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(i) => Some(f(i))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(b) => b
    }
    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
    def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)
    def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None
    else Some(xs.sum / xs. length)

  // while Option might seem like a sentinel value at first glance, the difference
  // is that it is dealt with by the caller at compile time rather than ambiguously
  // handled at run time.

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    val demeanseq = m match {
      case Some(n) => xs.map(Math.pow)
    }
  }
}
