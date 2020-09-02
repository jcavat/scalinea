package SchoolSchedule
import src.data._
import Day._

class TestLoader() extends DataLoader{
  val r = scala.util.Random
  r.setSeed(0)
  val maxPref: Int = 5
  val monday = new WorkDay(Monday, 4)
  val tuesday = new WorkDay(Tuesday, 4)

  val workDays = List(monday, tuesday)
  val periods = monday.periods ++ tuesday.periods

  val rooms = List(
    Room("1"),
    Room("2")
  )

  val a = Lecture("a", 2)
  val b = Lecture("b", 2)
  val c = Lecture("c", 2)

  val mata = Student("Mata", List(a, b))
  val nunes = Student("Nunes", List(b, c))

  val students = List(mata, nunes)

  val lectures = List(a, b, c)

  val teachers = List(
    Teacher("Albuquerque", (periods map {p => (p, r.nextInt(maxPref))}).toMap, List(a, b)),    
    Teacher("Cavat", (periods map {p => (p, r.nextInt(maxPref))}).toMap, List(c))
  )   

  def loadWorkDays(): List[WorkDay] = return workDays
  def loadTeachers(): List[Teacher] = return teachers
  def loadLectures(): List[Lecture] = return lectures
  def loadStudents(): List[Student] = return students
  def loadPeriods(): List[Period] = return periods
  def loadRooms(): List[Room] = return rooms
}