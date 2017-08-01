package euler
package p001_p020

object P014 {
  def answer: Int = {
    // The mutability makes me sad, but I can't find an efficient functional solution
    val cache: Array[Int] = Array.fill(1000000)(0)
    cache(1) = 1

    def next(n: Long): Long = {
      if (n % 2 == 0) n / 2
      else 3 * n + 1
    }

    def populate(n: Long): Int = {
      if (n >= 1000000)
        populate(next(n)) + 1
      else if (cache(n.toInt) != 0)
        cache(n.toInt)
      else {
        val res = populate(next(n)) + 1
        cache(n.toInt) = res
        res
      }
    }

    (1L until 1000000L).foreach(populate)

    var chainLength = 0
    var chainHead = 0
    for (i <- cache.indices) {
      if (cache(i) > chainLength) {
        chainLength = cache(i)
        chainHead = i
      }
    }
    chainHead
  }
}
