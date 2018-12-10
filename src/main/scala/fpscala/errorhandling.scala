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

  //   map2(optAge, optTickets)(insuranceRateQuote)
  // }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None}

  // Exercise 4.3 (answered incorrectly)
  // was there a way to do this in terms of map or flatmap?
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  object Option {
    // Exercise 4.4 (works properly) (answered incorrectly)
    // once again, I did pattern matching where flatMap could work instead.
    // I also cheated by using asInstanceOf to get the right type annotation.
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
    }

    // Exercise 4.5 (works properly) (answered incorrectly)

    // I didn't use map2. I'm not sure what issues that would cause in practice.
    // I did correctly realize that the identity function is the key to seqFromTraverse, though.
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

    def seqFromTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)((a) => a)
  }

  // Scala implements disjoint union types through the Either type - an idea with
  // broader applications that just error handling

  // exercise 4.6 (answered incorrectly)
  // once again I'm doing lots of pattern matching. it's hard for me to understand why this
  // isn't the correct way to do it, as the textbook doesn't really offer reasons why it's
  // more idiomatic (apart from maintainability, perhaps)

  // I felt reassured by getting it mostly right with pattern matching this time.
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e)  => Left(e)
      case Right(a) => Right(f(a))
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      a <- this
      bb <- b
    } yield f(a,bb)
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def meanEither(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list")
    else Right(xs.sum/xs.length)

  // using either to encapsulate the error caused by the code
  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x/y)
    catch {case e: Exception => Left(e)}

  def TryEither[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {case e: Exception => Left(e)}

  object Either {
    // exercise 4.7
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es match {
        case Nil => Right(Nil)

      }
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case h :: t => f(h) :: traverse(t)(f)
    }
  }

}
