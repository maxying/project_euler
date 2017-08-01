package euler
package p021_p040

object P024 {
  def answer: Long = {
    def divMod(start: Int, div: Int, acc: Seq[Int]): Seq[Int] = {
      if (start / div == 0) start % div +: acc
      else divMod(start / div, div + 1, start % div +: acc)
    }
    def pickDigits(take: Seq[Int], digits: List[Int], acc: List[Int]): List[Int] = {
      if (take.isEmpty) acc
      else {
        val idx = take.head
        pickDigits(take.tail, digits.take(idx) ++ digits.drop(idx + 1), digits(idx) +: acc)
      }
    }
    val takes = divMod(999999, 1, Seq())
    val digits = List(0,1,2,3,4,5,6,7,8,9)

    pickDigits(takes, digits, List()).reverse.mkString.toLong
  }
}
