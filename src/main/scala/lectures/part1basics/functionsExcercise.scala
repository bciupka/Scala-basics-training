package lectures.part1basics

object functionsExcercise extends App {

  def aGreetngsFunction(name: String, age: Int): String =
    "Hello, I'm " + name + " and my age is " + age

  println(aGreetngsFunction("Bartek", 25))

  def Factorial(n: Int): Int =
    if (n > 1) n * Factorial(n-1)
    else 1

  println(Factorial(5))

  def Fibonacci(n: Int): Int =
    if (n > 2) Fibonacci(n-1) + Fibonacci(n-2)
    else 1

  println(Fibonacci(2))

  def primeChecker(a: Int): Boolean = {
    val b = math.pow(a, 0.5)
    var c = 2
    if ((b % 1 == 0) || (a < 2)) false
    else {
      def devider(a1: Double): Boolean = {
        if (c < b) {
          if (a % c == 0) false
          else {
            c += 1
            devider(a1)
          }
        }
        else true
      }
      devider(a)
    }
  }

  println(primeChecker(37))

  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else n% t != 0 && isPrimeUntil(t-1)

    isPrimeUntil(n / 2)
  }

  println(isPrime(3))

  println(3.0/2)
  println(3/2)

}
