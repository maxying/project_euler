package euler
package p021_p040

object P025 {
  def answer: Int = {
    val cache: collection.mutable.HashMap[Int, BigInt] = collection.mutable.HashMap.empty

    def fib(n: Int): BigInt = {
      if (n <= 0) cache.getOrElseUpdate(n, 0)
      else if (n == 1) cache.getOrElseUpdate(n, 1)
      else cache.getOrElseUpdate(n - 1, fib(n - 1)) + cache.getOrElseUpdate(n - 2, fib(n - 2))
    }

    (for {
      i <- Stream.from(1)
      if fib(i).toString.length >= 1000
    } yield i).head
  }
}
