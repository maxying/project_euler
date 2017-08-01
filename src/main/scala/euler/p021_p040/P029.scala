package euler
package p021_p040

object P029 {
  def answer: Int = {
    val nums = for {
      a <- 2 to 100
      b <- 2 to 100
    } yield BigInt(a).pow(b)
    nums.toSet.size
  }
}
