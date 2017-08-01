package euler
package p021_p040

object P027 {
  def answer: Int = {
    val somePrimes = primesLeq(120000) // cached primes, estimated from 120,000 > 100*100 + 1000*100+1000
    val smallPrimes = somePrimes.takeWhile(_ < 1000)
    val primes = somePrimes.toSet

    def streamLength(a: Int, b: Int): Int = {
      def streamLengthLoop(n: Int, a: Int, b: Int): Int = {
        val quad = n * n + a * n + b
        if (primes.contains(quad) || isPrime(quad)) streamLengthLoop(n + 1, a, b)
        else n
      }
      streamLengthLoop(0, a, b)
    }
    val aStream = -999 to 999
    val bStream = smallPrimes ++ smallPrimes.map(_ * -1)
    val lengths = for {
      a <- aStream
      b <- bStream
    } yield (streamLength(a, b), a, b)
    val (_, a, b) = lengths.maxBy(_._1)
    a * b
  }
}
