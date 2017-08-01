package euler.p001_p020

object P009 {
  def answer: Int = {
    /*
      a + b + c = 1000
      a^2 + b^2 = 1,000,000 - 2000(a+b) + a^2 + 2ab + b^2
      1,000,000 - 2000(a+b) + 2ab = 0
      1000a+1000b - ab = 500,000
      a(1000-b) = 500000-1000b
      a = (500,000-1000b)/(1000-b)
    */
    val res = for {
      b <- 1 to 333
      if (500000 - 1000 * b) % (1000 - b) == 0
      a = (500000 - 1000 * b) / (1000 - b)
    } yield a * b * (1000 - a - b)
    res.head
  }
}
