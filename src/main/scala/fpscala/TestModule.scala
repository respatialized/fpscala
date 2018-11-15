package fpscala

import scala.annotation.tailrec

object TestModule {
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

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println("the factorial of %d is %d".format(7, factorial(7)))
    println("the %dth Fibonacci number is %d".format(4, fibo(4)))
  }
}
