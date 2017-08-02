package euler
package p001_p020

object P019 {
  def answer: Int = {
    def daysInMonth(month: Int, year: Int): Int = {
      if (month in Set(1,3,5,7,8,10,12)) 31
      else if (month in Set(4,6,9,11)) 30
      else if (isLeapYear(year)) 29
      else 28
    }
    def isLeapYear(year: Int): Boolean = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0

    var weekday = 1  // 1 Jan 1901 is a Tuesday
    (for {
      y <- 1901 to 2000
      m <- 1 to 12
      d <- 1 to daysInMonth(m, y)
      _ = weekday = (weekday + 1) % 7
      if weekday == 6 && d == 1
    } yield 1).length
  }
}
