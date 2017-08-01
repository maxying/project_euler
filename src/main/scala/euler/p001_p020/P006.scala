package euler
package p001_p020

object P006 {
  private def square(x: Int): Int = x * x
  def answer: Int = {
    val squareOfSums = square((1 to 100).sum)
    val sumOfSquares = (1 to 100).map(square).sum
    squareOfSums - sumOfSquares
  }
}
