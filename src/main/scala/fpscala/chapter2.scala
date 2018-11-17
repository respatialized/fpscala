package fpscala

import scala.annotation.tailrec

object Chapter2 {
  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n * acc)
    }
    go(n, 1)
  }

  def fibo(x: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)
    }
    go(x, 0, 1)
  }

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  // the input of a function can be any type we wish, including
  // a mapping from one type to another (which is another way of saying function)
  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }

  /* returns the index of the first occurence of key in sequence ss
   * or -1 if it's not there */
  def findFirst(ss: Seq[String], key: String): Int = {

    /* this helper function is a good example of thinking clearly about exactly what
     * you're iterating over in a recursive function. My first instinct would be to
     * iterate over the elements, but that leaves you without an index, which is the
     * whole point of writing the function in the first place! instead, the function
     * is iterating over the indices and using those to compare values */
    @tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    loop(0)
  }

  /* Writing a polymorphic version allows us to abstract over the input type as well as the
   * method by which we check the elements by putting the boolean logic into an input function */
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    /* I forgot to annotate the return type of the recursive function, which Scala doesn't allow. */
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  // solving the special case before generalizing - determines if it's an ascending list
  def isSorted(is: Seq[Int]): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= is.length - 1) true // nothing left to compare
      else if (is(n + 1) < is(n)) false
      else loop(n + 1)
    }
    loop(0)
  }

  /* recursively determines whether the given sequence is ordered according to the given fn */
  def isSorted[A](as: Seq[A], ordered: (A, A) => Boolean): Boolean = {
    // strategy: compare the nth and n+1th elements
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true // nothing left to compare
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

}
