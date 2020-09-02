package src.data
import Day._
case class WorkDay (day: Day, periodNumber: Int) {
  def this(day: String, periodNumber: Int) = this(Day.dayText(day), periodNumber)
 
  private val _periods: List[Period] = (for{ i <- 0 until periodNumber} yield Period(day, i)).toList
  def periods = _periods
  override def toString() : String = { return day.toString }
}