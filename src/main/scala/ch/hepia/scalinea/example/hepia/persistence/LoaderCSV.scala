package SchoolSchedule
import src.data._
import Day._
import LoaderMode._
import java.io._

class LoaderCSV() extends Loader{
  def loadVersion(filename: String, v: Scheduler) = {
    println(s"Loading schedule from schedule_versions/$filename.csv")
    val t0 = System.nanoTime

    //values to fill
    var periods: List[Period] = List()
    var workDays: List[WorkDay] = List()
    var rooms: List[Room] = List()
    var lectures: List[Lecture] = List()
    var students: List[Student] = List()
    var teachers: List[Teacher] = List()
    var dDayDensity: Double = 0.0
    var dDayLink: Double = 0.0
    var lectureToPeriods: Map[Lecture, List[Period]] = Map()
    var lectureToRoom: Map[Lecture, Room] = Map()
    var periodBreaks: List[Int] = List()
    var incompatibilities: List[List[Lecture]] = List()
    var links: List[List[Lecture]] = List()

    //data structures needed to load from CSV
    val bufferedSource = io.Source.fromFile(s"schedule_versions/$filename.csv")
    val lines = bufferedSource.getLines
    var mode : LoaderMode = LoadingWorkDays
    var tmpLectures: List[Lecture] = List()
    var tmpPreferences: Map[Period, Int] = Map()
    var tmpString: String = ""
    var tmpInt: Int = 0
    var tmpBool: Boolean = false

    for(line <- lines) {
      val columns = line.split(",").toList
      columns(0) match {
        case "workDays" => mode = LoadingWorkDays
        case "rooms" => mode = LoadingRooms
        case "lectures" => mode = LoadingLectures
        case "students" => mode = LoadingStudents
        case "teachers" => mode = LoadingTeachers
        case "dDayDensity" => mode = LoadingDDayDensity
        case "dDayLink" => mode = LoadingDDayLink
        case "lectureToPeriods" => mode = LoadingLectureToPeriods
        case "lectureToRoom" => mode = LoadingLectureToRoom
        case "periodBreaks" => mode = LoadingPeriodBreaks    
        case "incompatibilities" => mode = LoadingIncompatibilities
        case "links" => mode = LoadingLinks
        case _ => {
          mode match {
              case LoadingWorkDays => if(columns(0) == "") {
                val d = new WorkDay(columns(1), columns(2).toInt)
                workDays = workDays :+ d
                d.periods.foreach(p => { periods = periods :+ p})
              }
              case LoadingRooms => if(columns(0) == "") rooms = rooms :+ new Room(columns(1))
              case LoadingLectures => if(columns(0) == "") lectures = lectures :+ Lecture(columns(1), columns(2).replaceAll(" ", "").toInt)
              case LoadingStudents => { if(columns(0) == "") {
                  tmpLectures = List()
                  for(i <- 2 until columns.length)
                    tmpLectures = tmpLectures ++ lectures.filter(l => l.name == columns(i))
                  students = students :+ Student(columns(1), tmpLectures)
                }
              }
              case LoadingTeachers => if(columns(0) == "") {
                if(columns(1) == ""){
                  tmpPreferences = tmpPreferences + (periods.find(_.toString == columns(2)).get -> columns(3).toInt)
                } else {
                  for(i <- 4 until columns.length)
                    tmpLectures = tmpLectures ++ lectures.filter(l => l.name == columns(i))
                  val t = Teacher(columns(1), tmpPreferences, tmpLectures)
                  t.setMaxDailyPeriods(columns(2).replaceAll(" ", "").toInt)
                  if(columns(3).replaceAll(" ", "").toBoolean)
                    t.softenConstraints()
                  teachers = teachers :+ t
                }
              }
              case LoadingDDayDensity => dDayDensity = columns(1).toDouble
              case LoadingDDayLink => dDayLink = columns(1).toDouble
              case LoadingLectureToPeriods => {
                if(columns(0) == "") {
                  var ps : List[Period] = List()
                  for(i <- 2 until columns.length)
                    ps = ps :+ (periods.find(_.toString == columns(i))).get
                  lectureToPeriods = lectureToPeriods + (lectures.find(_.toString == columns(1)).get -> ps)
                }
              }
              case LoadingLectureToRoom => if(columns(0) == "") lectureToRoom = lectureToRoom + (lectures.find(_.toString == columns(1)).get -> rooms.find(_.toString == columns(2).replaceAll(" ", "")).get)
              case LoadingPeriodBreaks => if(columns(0) == "") for(i <- 1 until columns.length) periodBreaks = periodBreaks :+ columns(i).toInt
              case LoadingIncompatibilities => if(columns(0) == "") {tmpLectures = List(); for(i <- 1 until columns.length) tmpLectures = tmpLectures :+ lectures.find(_.toString == columns(i)).get; incompatibilities = incompatibilities :+ tmpLectures}
              case LoadingLinks => if(columns(0) == "") {tmpLectures = List(); for(i <- 1 until columns.length) tmpLectures = tmpLectures :+ lectures.find(_.toString == columns(i)).get; links = links :+ tmpLectures}
              case _ => println("ERROR")
          }
        }
      }
    }
    //assigning loaded values to schedule
    v.schedule.periods = periods
    v.schedule.workDays = workDays
    v.schedule.rooms = rooms
    v.schedule.lectures = lectures
    v.schedule.students = students
    v.schedule.teachers = teachers
    v.schedule.dDayLink = dDayLink
    v.schedule.lectureToPeriods = lectureToPeriods
    v.schedule.lectureToRoom = lectureToRoom
    v.schedule.periodBreaks = periodBreaks
    v.schedule.incompatibilities = incompatibilities
    v.schedule.links = links
    val saveDuration = (System.nanoTime - t0) / 1e9d
    println(s"Version $filename loaded in $saveDuration s")
  }
}
