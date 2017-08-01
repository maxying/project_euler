package euler
package p001_p020

object P002 {
  def answer: Long = {
    // Fib #s are even with period 3
    val evenFibs = for (i <- Stream.from(0, step = 3)) yield fib(i)
    evenFibs.takeWhile(_ <= 4e6).sum
  }
}
