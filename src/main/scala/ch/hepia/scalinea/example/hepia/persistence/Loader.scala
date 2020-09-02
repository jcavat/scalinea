package SchoolSchedule
import src.data._
trait Loader {
  def loadVersion(filename: String, v: Scheduler): Unit
}