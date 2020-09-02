package SchoolSchedule
import src.data._
import Day._

class DemoLoader() extends DataLoader{

  val monday = WorkDay(Monday, 9)
  val tuesday = WorkDay(Tuesday, 9)
  val wednesday = WorkDay(Wednesday, 9)
  val thursday = WorkDay(Thursday, 9)
  val friday = WorkDay(Friday, 9)

  val workDays = List(monday, tuesday, wednesday, thursday, friday)
  val periods = monday.periods ++ tuesday.periods ++ wednesday.periods ++ thursday.periods ++ friday.periods

  val rooms = List(
    Room("501"),
    Room("502")
  )

  val a = new Lecture("Mathématiques 1", 2)
  val b = new Lecture("Mathématiques 2", 4)
  val c = new Lecture("POO Java 1", 2)
  val d = new Lecture("POO Java 2", 4)
  val e = new Lecture("Bases de données 1", 2)
  val f = new Lecture("Bases de données 2", 4)
  val g = new Lecture("Web 1", 2)
  val h = new Lecture("Web 2", 4)
  val i = new Lecture("Programmation 1", 2)
  val j = new Lecture("Programmation 2", 4)
  val k = new Lecture("Sécurité 1", 2)
  val l = new Lecture("Sécurité 2", 4)
  val m = new Lecture("Compilation 1", 2)
  val n = new Lecture("Compilation 2", 4)
  val o = new Lecture("Vision numérique 1", 2)
  val p = new Lecture("Vision numérique 2", 4)

  val mata = Student("Mata", List())
  val nunes = Student("Nunes", List())
  val heirich = Student("Heirich", List())

  val students = List(mata, nunes, heirich)

  val lectures = List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

  val teachers = List(
    Teacher("Cavat", (periods map {p => (p, if(p.number <5) 0 else 1)}).toMap, List(a, b, c, d, e, f, g, h)),    
    Teacher("Albuquerque", (periods map {p => (p, if(p.number < 8) 1 else 0)}).toMap, List(i, j, k, l, m, n, o, p))    
  )   

  def loadWorkDays(): List[WorkDay] = {
    return workDays
  }
  def loadTeachers(): List[Teacher] = {
    return teachers
  }
  def loadLectures(): List[Lecture] = {
    return lectures
  }
  def loadStudents(): List[Student] = {
    return students
  }
  def loadPeriods(): List[Period] = {
    return periods
  }
  def loadRooms(): List[Room] = {
    return rooms
  }
}
