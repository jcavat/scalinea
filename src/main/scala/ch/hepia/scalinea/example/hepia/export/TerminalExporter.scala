package SchoolSchedule
import java.io._

class TerminalExporter() extends Exporter {
  
  def writeFile(path: String, a: Array[Array[String]]) = {
    val padding = "%1$-9s"
    for(column <- a){
      for(cell <- column){
        print(padding.format(cell.take(9)))
        print("|")
      }
      println("")
    }
  }
}
             