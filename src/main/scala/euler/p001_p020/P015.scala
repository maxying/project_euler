package euler
package p001_p020

object P015 {
  def answer: Long = {
    val cache = Array.fill(22)(Array.fill(22)(0L))
    for (i <- 1 to 21) {
      cache(1)(i) = 1
      cache(i)(1) = 1
    }
    def loop(r: Int, c: Int): Long = {
      if (cache(r)(c) != 0) cache(r)(c)
      else {
        val res = loop(r - 1, c) + loop(r, c - 1)
        cache(r)(c) = res
        res
      }
    }
    loop(21, 21)
  }
}
