package euler
package p001_p020

object P016 {
  def answer: Int = {
    val n = BigInt(2).pow(1000)
    n.toString.map(_.asDigit).sum
  }
}
