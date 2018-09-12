package ch.hepia.scalinea.util

object MathUtil {
  val delta: Double = 0.00001
  def nonZero(value: Double): Boolean = math.abs(value) > delta
}
