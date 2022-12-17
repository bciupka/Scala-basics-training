package lectures.part1basics

import scala.annotation.tailrec

object recursionExcercise extends App {
  @tailrec
  def tailFact (n: BigInt, accumulator: BigInt): BigInt =
    if (n > 1) tailFact(n-1, n * accumulator)
    else accumulator


  println(tailFact(50,1))

  def concatenateString (s: String, amount: Int):String = {
    @tailrec
    def concatenateStringAccu(n: Int, accu: String): String =
      if (n > 1) concatenateStringAccu(n-1, accu + s)
      else accu
    concatenateStringAccu(amount, s)
  }

  println(concatenateString("Wow ", 20))


  def isPrime (a: Int): Boolean = {
    val b = a / 2
    if (b < 2 && b > 0) true
    else if (b <= 0) false
    else {
      @tailrec
      def isPrimeAccu(c: Int): Boolean =
        if (c <= b && a % c != 0) isPrimeAccu(c + 1)
        else if (c <= b) false
        else true
      isPrimeAccu(2)
    }
  }

  println(isPrime(567 * 45))

  /*KATASTROFA
  def fibonacci (n: BigInt): BigInt = {
    if (n <= 2) 1
    else {
      @tailrec
      def fibAccu(n1:BigInt, accu: BigInt):BigInt = {
        if (n1 < n) fibAccu(n1 + 1, accu + fibonacci(n1-1))
        else accu
      }
    fibAccu(3, 2)
    }
  }

  println(fibonacci(8))
  */

  def isPrime2 (n: Int): Boolean = {
    @tailrec
    def isPrimeAccu (l: Int, accu: Boolean): Boolean = {
      if (l <= n / 2 && accu) isPrimeAccu(l + 1, n % l != 0)
      else if (!accu || n < 2) false
      else true
    }
    isPrimeAccu(2, true)
  }

  println(isPrime2(3))

  def isPrimeFromVid (n :Int): Boolean = {
    @tailrec
    def isPrime3(t: Int, isStill: Boolean): Boolean =
      if (!isStill) false
      else if (t <= 1) true
      else isPrime3(t - 1, n % t != 0 && isStill)
    isPrime3(n/2, true)
  }

  println(isPrimeFromVid(1)) // No i kapa :D

  def fibonacci (n: BigInt): BigInt = {
    @tailrec
    def fibAccu (n1: BigInt, accu: BigInt, accu2: BigInt): BigInt =
      if (n1 < n) fibAccu(n1 + 1, accu2, accu + accu2)
      else if (n > 2) accu2 + accu
      else accu
    fibAccu(3, 1, 1)
  }

  println(fibonacci(100))

  def fibFromVid (n: Int): Int = {
    @tailrec
    def fibo(i: Int, last: Int, last2:Int): Int =
      if (i >= n) last
      else fibo(i + 1, last + last2, last)

    if (n <= 2) 1
    else fibo(2 ,1 ,1)
  }


  println(fibFromVid(3))
}
