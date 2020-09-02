package SchoolSchedule
import src.data._
trait DataLoader {
  def loadTeachers(): List[Teacher]
  def loadLectures(): List[Lecture]
  def loadStudents(): List[Student]
  def loadWorkDays(): List[WorkDay]
  def loadPeriods(): List[Period]
  def loadRooms(): List[Room]
}