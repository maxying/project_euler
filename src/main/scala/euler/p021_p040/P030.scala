package euler
package p021_p040

object P030 {
  def answer: Int = {
    def fifthPow(n: Int) = pow(n, 5)
    def sumDigits5thPower(n: Int): Int = n.toString.map(c => fifthPow(c.asDigit)).sum

    val limit = 5 * fifthPow(9)
    val validNums = Stream.range(2, limit + 1).filter(i => i == sumDigits5thPower(i))
    validNums.sum
  }
}
