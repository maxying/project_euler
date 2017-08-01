package euler
package p001_p020

object P003 {
  def answer: Long = {
    val n = 600851475143L
    primeFactors(n).head
  }
}
