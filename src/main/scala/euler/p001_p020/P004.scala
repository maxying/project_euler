package euler
package p001_p020

object P004 {
  def answer: Int = {
    val palindromes = for {
      i <- 999 to 900 by -1
      j <- i to 900 by -1
      prod = i * j
      if prod.toString == prod.toString.reverse
    } yield prod
    palindromes.max
  }
}
