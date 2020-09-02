package SchoolSchedule
import src.data._
import java.io._

trait Exporter {
  protected def writeFile(path: String, a: Array[Array[String]]) : Unit
  
  def exportSchedule(name: String, m : Map[Lecture, List[Period]], wd: List[WorkDay]) : Unit = {
    val lineSize = wd.size + 1
    val columnSize = wd.head.periods.size + 1
    var array = Array.ofDim[String](columnSize, lineSize)
    //write periods number on first column, days on first line
    for(column <- 0 until lineSize){
      for(line <- 0 until columnSize){
        if(line == 0 && column > 0)
          array(line)(column) = wd(column-1).toString
        else if(column == 0 && line > 0)
          array(line)(column) = "h" ++ wd.head.periods(line-1).number.toString
        else
          array(line)(column) = ""
      }
    }
    array(0)(0) = name
    
    //write classes assigned in schedule
    for ((l, ps) <- m) {
      for(p <- ps){
        for(d <- wd){
          if(d.periods.contains(p))
            array(d.periods.indexOf(p)+1)(wd.indexOf(d)+1) = l.toString
        }
      }
    }
    writeFile(s"output/Schedule_$name", array)
  }
}