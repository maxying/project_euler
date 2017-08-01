package euler.p021_p040

object P022 {
  def answer: Int = {
    def alphaVal(name: String): Int = name.map(_ - 'A' + 1).sum
    val source = scala.io.Source.fromResource("p022names.txt")
    val lines = try source.mkString finally source.close()
    val names = lines.split(",").map(_.replace("\"", ""))
    names.sorted.zipWithIndex.map {
      case (name, idx) => (idx + 1) * alphaVal(name)
    }.sum
  }
}
