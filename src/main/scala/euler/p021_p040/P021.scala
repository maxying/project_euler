package euler
package p021_p040

object P021 {
  def answer: Int = {
    def d(n: Int): Int = sumDivisors(n) - n // proper divisors only

    val amicables = for {
      n <- 1 to 10000
      m = d(n)
      if d(m) == n && m != n
    } yield (m, n)

    amicables.map{ case (m, n) => m + n }.sum / 2  // We double count (m, n) and (n, m)
  }
}
