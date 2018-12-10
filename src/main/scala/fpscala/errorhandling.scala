// package fpscala.errorhandling

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

  // Exercise 4.2 (answered incorrectly)
  // just because I put a flatMap in the answer, it doesn't mean I implemented variance
  // in terms of flatMap. The presence of flatMap across the standard library as a method
  // on most objects and classes made me forget that in the context of this chapter, it's
  // just a method on the Option type defined in these exercises.
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map{x => Math.pow(x - m, 2)}))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _.map(f)

  // def parseInsuranceRateQuote(
  //   age: String,
  //   numberOfSpeedingTickets: String
  // ): Option[Double] = {
  //   val optAge: Option[Int] = Try(age.toInt)
  //   val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

  //   insuranceRateQuote(optAge, optTickets)
  // }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None}

  // Exercise 4.3 (answered incorrectly)
  // was there a way to do this in terms of map or flatmap?
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap{aa =>
    b.map(bb => f(aa,bb))
  }
}
