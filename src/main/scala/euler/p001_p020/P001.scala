package euler
package p001_p020

import scala.math.ceil

object P001 {
  def answer: Int = {
    val multsOf3 = (1 until ceil(1000d / 3).toInt).sum * 3
    val multsOf5 = (1 until ceil(1000d / 5).toInt).sum * 5
    val multsOf15 = (1 until ceil(1000d / 15).toInt).sum * 15
    // Inclusion-Exclusion
    multsOf3 + multsOf5 - multsOf15
  }
}
