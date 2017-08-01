package euler
package p001_p020

object P012 {
  def answer: Int = {
    def numFactors(n: Int): Int = {
      val step = if (n % 2 == 1) 2 else 1
      val doubleCountAdjustment = if (isSquare(n)) -1 else 0
      (1 to math.sqrt(n).toInt by step).count(n % _ == 0) * 2 + doubleCountAdjustment
    }
    (for {
      i <- Stream.from(1)
      n = i * (i + 1) / 2
      div = if (i % 2 == 0) numFactors(i / 2) * numFactors(i + 1) else numFactors(i) * numFactors((i + 1) / 2)
      if div >= 500
    } yield n).head
  }
}
