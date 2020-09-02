package SchoolSchedule
import src.data._
import Day._

class TmpLoader() extends DataLoader{
  val r = scala.util.Random
  r.setSeed(0)
  val maxPref: Int = 3
  val monday = new WorkDay(Monday, 9)
  val tuesday = new WorkDay(Tuesday, 9)
  val wednesday = new WorkDay(Wednesday, 9)
  val thursday = new WorkDay(Thursday, 9)
  val friday = new WorkDay(Friday, 9)

  val workDays = List(monday, tuesday, wednesday, thursday, friday)
  val periods = monday.periods ++ tuesday.periods ++ wednesday.periods ++ thursday.periods ++ friday.periods

  val rooms = List(
    new Room(1)
  )

  val a = new Lecture("a")
  val b = new Lecture("b")
  val c = new Lecture("c")
  val d = new Lecture("d")
  val e = new Lecture("e")
  val f = new Lecture("f", 3)
  val g = new Lecture("g")
  val h = new Lecture("h")

  val mata = new Student("Mata", List(a, h))
  val nunes = new Student("Nunes", List(b, g))
  val heirich = new Student("Heirich", List(b, f))

  val students = List(mata, nunes, heirich)

  val lectures = List(a, b, c, d, e, f, g, h)

  val teachers = List(
    new Teacher("Malaspinas", (periods map {p => (p, r.nextInt(maxPref))}).toMap, List(a, b, c, d)),    
    new Teacher("Cavat", (periods map {p => (p, r.nextInt(maxPref))}).toMap, List(e, f, g, h)),    
    /*new Teacher("Albuquerque", (periods map {p => (p, r.nextInt(maxPref))}).toMap, List(e)),    
    new Teacher("Hoerdt", (periods map {p => (p, r.nextInt(maxPref))}).toMap, List(f)),    
    new Teacher("Gluck", (periods map {p => (p, r.nextInt(maxPref))}).toMap, List(g)),    
    new Teacher("Falcone", (periods map {p => (p, r.nextInt(maxPref))}).toMap, List(h))*/
  )   

  def loadWorkDays(): List[WorkDay] = workDays
  def loadTeachers(): List[Teacher] = teachers
  def loadLectures(): List[Lecture] = lectures
  def loadStudents(): List[Student] = students
  def loadPeriods(): List[Period] = periods
  def loadRooms(): List[Room] = rooms
}
