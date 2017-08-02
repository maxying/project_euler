import scala.annotation.tailrec
import scala.math.{log, sqrt}

package object euler {

  /* ********** UTILITY ********** */

  implicit class InStuff[T](val x: T) extends AnyVal {
    def in(coll: Seq[T]): Boolean = coll.contains(x)
    def in(coll: Set[T]): Boolean = coll.contains(x)
    def in(coll: Map[T, _]): Boolean = coll.contains(x)
  }

  def timeit[T](thunk: => T): T = {
    val start = System.nanoTime()
    val res = thunk
    val end = System.nanoTime()
    println(s"Time ${(end - start) / 1000} ms")
    res
  }


  /* ********** MATH ********** */

  /** Power for integers when math.pow is overkill */
  def pow(n: Int, p: Int): Int = {
    if (p == 0)          1
    else if (p == 1)     n
    else if (p % 2 == 0) pow(n * n, p / 2)
    else                 n * pow(n * n, p / 2)
  }
  def pow(n: Long, p: Long): Long = {
    if (p == 0)          1
    else if (p == 1)     n
    else if (p % 2 == 0) pow(n * n, p / 2)
    else                 n * pow(n * n, p / 2)
  }

  /** F_0 = 0; F_1 = 1 */
  def fib(n: Int): Long = {
    @tailrec
    def loop(a: Long, b: Long, n: Int): Long = {
      if (n <= 0) a
      else loop(b, a + b, n - 1)
    }
    loop(0, 1, n)
  }

  /**
    * Returns the prime factors of a number as a decreasing sequence.
    * Does not carry any info about multiplicity.
    */
  def primeFactors(n: Long): IndexedSeq[Int] = {
    @tailrec
    def loop(n: Long, start: Int, acc: IndexedSeq[Int]): IndexedSeq[Int] = {
      if (n == 1) acc
      else {
        val divvedOut = for (f <- Stream.from(start) if n % f == 0) yield (f, divOutFactor(n, f))
        val (prime, smaller) = divvedOut.head
        loop(smaller, prime, prime +: acc)
      }
    }
    loop(n, 2, Vector())
  }

  @tailrec
  def divOutFactor(n: Long, factor: Long): Long = {
    if (n % factor == 0) divOutFactor(n / factor, factor)
    else n
  }

  def isSquare(n: Long): Boolean = {
    val root = sqrt(n).toLong
    root * root == n
  }

  def factors(n: Long): IndexedSeq[(Long, Long)] = {
    val step = if (n % 2 == 1) 2 else 1
    for {
      p <- 1 to math.sqrt(n.toDouble).toInt by step
      if n % p == 0
    } yield (p.toLong, n / p)
  }

  def sumDivisors(n: Int): Int = {
    val sumFactors = factors(n).map{ case (a, b) => a + b }.sum
    val doubleCountFactor = if (isSquare(n)) math.sqrt(n).toInt else 0
    sumFactors.toInt - doubleCountFactor
  }

  def isPrime(n: Long): Boolean = {
    if (n == 2) true
    else if (n < 2 || n % 2 == 0) false
    else factors(n).length == 1
  }

  def primesLeq(n: Int): IndexedSeq[Int] = {
    val sieve = Array.fill(n + 1)(true)
    sieve(0) = false
    sieve(1) = false

    for {
      i <- 2 to sqrt(n).toInt if sieve(i)
      j <- i * i to n by i
    } {
      sieve(j) = false
    }

    for (i <- 2 to n if sieve(i)) yield i
  }

  def nthPrime(n: Int): Int = {
    // Prime number theorem: p(n) < n(ln(n) + ln(ln(n))) for n >= 6
    val p_5 = 11
    val upper = if (n >= 6) (n * (log(n) + log(log(n)))).toInt else p_5
    primesLeq(upper)(n - 1)
  }

  def factorial(n: Int): BigInt = {
    @tailrec
    def loop(n: Int, acc: BigInt): BigInt = {
      if (n <= 1) acc
      else loop(n - 1, n * acc)
    }
    loop(n, 1)
  }
}
