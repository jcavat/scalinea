package SchoolSchedule
import java.io._

class CsvExporter() extends Exporter {
  
  def writeFile(path: String, a: Array[Array[String]]) = {
    val pw = new PrintWriter(new File(s"output/Schedule_$path.csv"))
    for(field <- a){
      for(record <- field)
        pw.write(record + ",")
      pw.write("\n")
    }
    pw.close
  }
}
