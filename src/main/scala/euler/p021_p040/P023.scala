package euler
package p021_p040

object P023 {
  def answer: Int = {
    def isAbundant(n: Int): Boolean = sumDivisors(n) > 2 * n
    val abundantNums = scala.collection.SortedSet((12 to 28123).filter(isAbundant): _*)
    val notAbundantPairSums = (1 to 28123).filter {
      n => {
        val smallerAbundantNums = abundantNums.until(n)
        smallerAbundantNums.forall {
          k => !abundantNums.contains(n - k)
        }
      }
    }
    notAbundantPairSums.sum
  }
}
