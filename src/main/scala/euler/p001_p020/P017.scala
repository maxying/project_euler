package euler
package p001_p020

object P017 {
  def answer: Int = {
    val lt20 = Map(0 -> "", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five",
      6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten",
      11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen",
      16 -> "sixteen", 17 -> "seventeen", 18 -> "eighteen", 19-> "nineteen")
    val multsOf10 = Map(20 -> "twenty", 30 -> "thirty", 40 -> "forty", 50 -> "fifty",
      60 -> "sixty", 70 -> "seventy", 80 -> "eighty", 90 -> "ninety")
    val mags = Map(100 -> "hundred", 1000 -> "thousand")

    def asWord(n: Int): String = {
      val thousands = n / 1000
      val thousandsStr = lt20(thousands) + (if (thousands > 0) mags(1000) else "")

      val n2 = n % 1000
      val hundreds = n2 / 100
      val hundredsStr = lt20(hundreds) + (if (hundreds > 0) mags(100) else "")

      val n3 = n2 % 100
      val smallStr = if (n3 < 20) lt20(n3) else multsOf10(n3 - (n3 % 10)) + lt20(n3 % 10)
      val andStr = if (hundredsStr != "" && smallStr != "") "and" else ""

      thousandsStr + hundredsStr + andStr + smallStr
    }
    (1 to 1000).map(asWord(_).length).sum
  }
}
