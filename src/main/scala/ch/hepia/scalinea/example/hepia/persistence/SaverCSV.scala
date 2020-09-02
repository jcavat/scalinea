package SchoolSchedule
import src.data._
import Day._
import java.io._

class SaverCSV() extends Saver{
  def saveVersion(filename: String, v: Scheduler) = {
    println(s"Saving schedule to schedule_verions/$filename.csv")
    val t0 = System.nanoTime
    val pw = new PrintWriter(new File("schedule_versions/" + filename + ".csv"))

    pw.write("workDays\n")
    v.schedule.workDays.foreach(d => pw.write(s",${d.day},${d.periodNumber}\n"))
    pw.write("rooms\n")
    v.schedule.rooms.foreach(r => pw.write(s",${r.id}\n"))
    pw.write("lectures\n")
    v.schedule.lectures.foreach(l => pw.write(s",${l.name}, ${l.duration}\n"))
    pw.write("students\n")
    v.schedule.students.foreach(s => {
      pw.write(s",${s.name}")
      s.subscriptions.foreach(l => pw.write(s",$l"))
      pw.write("\n")
    })
    pw.write("teachers")
    v.schedule.teachers.foreach(t => {
      t.preferences foreach {case (period, preference) => pw.write(s"\n,,$period,$preference")}
      pw.write(s"\n,${t.name}, ${t.maxDailyPeriods}, ${t.isConstraintSoftened}")
      t.lectures.foreach(l => pw.write(s",$l"))
    })
    pw.write(s"dDayLink, ${v.schedule.dDayLink}\n")
    pw.write("lectureToPeriods\n")
    v.schedule.lectureToPeriods foreach {case (lecture, periods) => {
      pw.write(s",$lecture")
      periods.foreach(p => pw.write(s",$p"))
      pw.write("\n")
    }}
    pw.write("lectureToRoom\n")
    v.schedule.lectureToRoom foreach {case (lecture, room) => pw.write(s",$lecture, $room\n")}
    pw.write("periodBreaks\n")
    v.schedule.periodBreaks.foreach(b => pw.write(s",$b"))
    pw.write("\n")
    pw.write("incompatibilities\n")
    v.schedule.incompatibilities.foreach(il => {
      il.foreach(l => pw.write(s",$l"))
      pw.write("\n")
    })
    pw.write("links\n")
    v.schedule.links.foreach(ll => {
      ll.foreach(l => pw.write(s",$l"))
      pw.write("\n")
    })    
    pw.close
    val saveDuration = (System.nanoTime - t0) / 1e9d
    println(s"Version $filename saved in $saveDuration s")
  }
}
