package euler
package p021_p040

object P026 {
  def answer: Int = {
    def terminating(n: Int): Boolean = primeFactors(n).forall(i => i == 2 || i == 5)

    def relPrimeTo10(n: Int): Int = divOutFactor(divOutFactor(n, 2), 5).toInt
    def decimalCycle(n: Int): Int = {
      def decimalCycleLoop(n: Int, acc: Int): Int = {
        if (BigInt(10).pow(acc) % n == 1)
          acc
        else
          decimalCycleLoop(n, acc + 1)
      }
      if (terminating(n)) 0
      else decimalCycleLoop(relPrimeTo10(n), 1)
    }

    val cycleLengths = (1 until 1000).map(decimalCycle)
    val maxLength = cycleLengths.max
    cycleLengths.indexWhere(_ == maxLength) + 1
  }
}
