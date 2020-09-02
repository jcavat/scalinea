package SchoolSchedule
import src.data._
import Day._
import org.apache.poi.ss.usermodel.{ DataFormatter, WorkbookFactory, Row }
import java.io.File

class ItiLoader(teacherNumber: Int) extends DataLoader {

  def fromTeachersConstraints() = {
    val formatter = new DataFormatter()
    val teacherPrefFile = new File("input/ITI_contraintes_horaires_automne_2019.xlsx")
    val teacherPrefWb = WorkbookFactory.create(teacherPrefFile)
    val teacherPrefSheet = teacherPrefWb.getSheetAt(1)
    teacherPrefSheet.forEach(row => {
      val maybeName = Option(row.getCell(0, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL))
      if (maybeName.isDefined && maybeName.get.toString != "P. Nom") {
        var pref: Map[Period, Int] = Map()
        for (i <- 0 to 15) {
          val d = workdays((i - 1) / 3)
          val maybePref = Option(row.getCell(i + 1, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL))
          if (maybePref.isDefined) {
            pref = pref + (d.periods(4) -> 0)
            val p = formatter.formatCellValue(maybePref.get).toInt;
            i % 3 match {
              case 0 => {
                pref = pref + (d.periods(0) -> p); pref = pref + (d.periods(1) -> p); pref = pref + (d.periods(2) -> p); pref = pref + (d.periods(3) -> p)
              }
              case 1 => {
                pref = pref + (d.periods(5) -> p); pref = pref + (d.periods(6) -> p); pref = pref + (d.periods(7) -> p); pref = pref + (d.periods(8) -> p)
              }
              case 2 => {
                pref = pref + (d.periods(9) -> p); pref = pref + (d.periods(10) -> p); pref = pref + (d.periods(11) -> p); pref = pref + (d.periods(12) -> p); pref = pref + (d.periods(13) -> p); pref = pref + (d.periods(14) -> p)
              }
            }
          }
        }
        teachers = teachers :+ Teacher(maybeName.get.toString, pref, List())
      }
    })
  }

  def fromStudyPlan() = {
    val formatter = new DataFormatter()
    val planFile = new File("input/Plan_etudes_ITI_2019_20.xlsx")
    val planWb = WorkbookFactory.create(planFile)
    val planSheet = planWb.getSheetAt(0)
    var i = 0

    planSheet.forEach(row => {
      val maybeLecture = Option(row.getCell(4, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL))
      val maybeDuration = Option(row.getCell(26, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL))
      val maybeTeacher = Option(row.getCell(37, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL))
      val maybeRoom = Option(row.getCell(6, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL))
      if (maybeLecture.isDefined && maybeDuration.isDefined && maybeTeacher.isDefined) {
        val l = Lecture(maybeLecture.get.toString, formatter.formatCellValue(maybeDuration.get).toInt)
        var teacherName = maybeTeacher.get.toString.split(";").toList.head
        if (teacherName(teacherName.size - 1) == ')')
          teacherName = teacherName.dropRight((teacherName.size - 1) - (teacherName.indexOf("(") - 2))
        if (teacherName != "Enseignant-e-s" && !teacherName.startsWith("Vac.")) {
          lectures = lectures :+ l
          val teacher = teachers.filter(_.name == teacherName)
          if (teacher.isEmpty)
            println(s"ERROR: teacher missing : $teacherName")
          else
            teacher.foreach(t => {
              val test = lectures.filter(_.name == l.name)
              if (!test.isEmpty)
                t.lectures = t.lectures :+ test.head
            })
          if (maybeRoom.isDefined) {
            val r = Room(maybeRoom.get.toString.split(";").toList.head)
            if (!rooms.contains(r))
              rooms = rooms :+ r
            roomForLecture = roomForLecture + (l -> r)
          }
        }
      }
    })
  }

  //data loader
  var students: List[Student] = List()
  var lectures: List[Lecture] = List()
  var teachers: List[Teacher] = List()
  var rooms: List[Room] = List()
  val workdays: List[WorkDay] = List(WorkDay(Monday, 15), WorkDay(Tuesday, 15), WorkDay(Wednesday, 15), WorkDay(Thursday, 15), WorkDay(Friday, 15))
  val periods: List[Period] = workdays(0).periods ++ workdays(1).periods ++ workdays(1).periods ++ workdays(2).periods ++ workdays(3).periods ++ workdays(4).periods

  //assignations
  var roomForLecture: Map[Lecture, Room] = Map()
  val t0 = System.nanoTime

  //parsing input files
  fromTeachersConstraints
  fromStudyPlan
  val parsingDuration = (System.nanoTime - t0) / 1e9d
  println(s"parsing done in $parsingDuration s")

  def loadTeachers(): List[Teacher] = {
    var ts: List[Teacher] = List()
    for (i <- 0 until teacherNumber) {
      if (!teachers(i).lectures.isEmpty)
        ts = ts :+ teachers(i)
    }
    ts
  }

  def loadLectures(): List[Lecture] = {
    var ls: List[Lecture] = List()
    for (i <- 0 until teacherNumber) {
      if (!teachers(i).lectures.isEmpty) {
        teachers(i).lectures.foreach(l => {
          ls = ls :+ l
        })
      }
    }
    ls
  }

  def loadStudents(): List[Student] = students

  def loadWorkDays(): List[WorkDay] = workdays

  def loadPeriods(): List[Period] = periods

  def loadRooms(): List[Room] = rooms

}