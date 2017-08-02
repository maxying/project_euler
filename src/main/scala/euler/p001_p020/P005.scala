package euler
package p001_p020

object P005 {
  def answer: Int = {
    val n = 20
    def multiplicity(prime: Int, n: Int): Int = {
      if (prime > n) 0
      else 1 + multiplicity(prime, n / prime)
    }
    val prod = for (prime <- primesLeq(20)) yield (prime, multiplicity(prime, n))
    prod.foldLeft(1){ case (acc, (p, deg)) => acc * pow(p, deg) }
  }
}
