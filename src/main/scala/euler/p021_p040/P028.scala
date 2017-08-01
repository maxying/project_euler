package euler
package p021_p040

object P028 {
  def answer: Long = {
    def odd2nth(n: Long): Long = (n + 1) / 2
    def corner(n: Long): Long = {
      val k = odd2nth(n)
      4 * k * k - 10 * k + 7 // OEIS A054554
    }
    def layer(n: Long): Long = if (n == 1) 1 else 4 * corner(n) + 6 * (n - 1)
    def total(n: Long): Long = if (n == 1) layer(n) else layer(n) + total(n - 2)
    total(1001)
  }
}
