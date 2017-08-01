package euler
package p001_p020

object P020 {
  def answer: Int = factorial(100).toString.map(_.asDigit).sum
}
