package euler
package p001_p020

object P010 {
  def answer: Long = {
    primesLeq(2000000).map(_.toLong).sum
  }
}
