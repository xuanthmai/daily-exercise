import java.io._

object LPrimeFactors { 
  /*
   * Complete the 'primeCount' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts LONG_INTEGER n as parameter.
   */
  def primeCount(n: Long): Int = {
    var product: Long = 1
    var countPrime: Int = -1

    // Some edge cases
    if (n == 1) 0
    else if (n == 2 || n == 3) 1
    else {
      var i: Long = 1
      do {
        if (isPrime(i)) {
          countPrime += 1
          product *= i
        }
        i += 1
      } while (product < n && product > 0) // < 0 to prevent overflow

      if (product == n) countPrime + 1 // edge case
      else countPrime
    }
  }

  def isPrime(n: Long): Boolean = {
    if (n == 1) false
    else if (n == 2 || n == 3) true
    else if (n % 2 == 0 || n % 3 == 0) false
    else {
      val isDivisible = for {
        i <- 4 to Math.sqrt(n.toDouble).floor.toInt
        isP = if (n % i == 0) false else true 
      } yield isP

      isDivisible.forall(_ == true)
    }
  }

  def main(args: Array[String]): Unit = {
    // 0
    println(primeCount(1))
    // 1
    println(primeCount(2))
    // 1
    println(primeCount(3))
    // 6 
    println(primeCount(100000))
    // 15
    println(primeCount(1000000000000000000L))
    // 15
    println(primeCount(999999999999969970L))
    // 3
    println(primeCount(30))
  }
}

