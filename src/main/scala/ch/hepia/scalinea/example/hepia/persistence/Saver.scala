package SchoolSchedule
import src.data._
trait Saver {
  def saveVersion(filename: String, v: Scheduler) : Unit
}