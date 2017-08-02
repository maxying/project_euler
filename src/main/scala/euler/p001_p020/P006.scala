package euler
package p001_p020

object P006 {
  def answer: Int = {
    def square(n: Int) = pow(n, 2)
    val squareOfSums = square((1 to 100).sum)
    val sumOfSquares = (1 to 100).map(square).sum
    squareOfSums - sumOfSquares
  }
}
