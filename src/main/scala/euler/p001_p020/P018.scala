package euler
package p001_p020

object P018 {
  def answer: Int = {
    val triStr = """75
                   |95 64
                   |17 47 82
                   |18 35 87 10
                   |20 04 82 47 65
                   |19 01 23 75 03 34
                   |88 02 77 73 07 63 67
                   |99 65 04 28 06 16 70 92
                   |41 41 26 56 83 40 80 70 33
                   |41 48 72 33 47 32 37 16 94 29
                   |53 71 44 65 25 43 91 52 97 51 14
                   |70 11 33 28 77 73 17 78 39 68 17 57
                   |91 71 52 38 17 14 91 43 58 50 27 29 48
                   |63 66 04 68 89 53 67 30 73 16 69 87 40 31
                   |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23""".stripMargin

    val triMat = triStr.split("\n").map{ s => s.split(" ").map(_.toInt) }

    def loop(r:Int, c: Int): Int = {
      triMat(r)(c) + (
        if (r != 0) {
          if (c == 0) loop(r - 1, c)
          else if (c == r) loop(r - 1, c - 1)
          else math.max(loop(r - 1, c - 1), loop(r - 1, c))
        } else 0)
    }
    (0 to 14).map(loop(14, _)).max
  }
}
