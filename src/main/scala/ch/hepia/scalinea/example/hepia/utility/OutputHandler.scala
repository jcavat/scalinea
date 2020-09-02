package SchoolSchedule
import java.io._

object OutputHandler{
    val originalStream: PrintStream = System.out
    val dummy: PrintStream = new PrintStream(new OutputStream(){ def write (b: Int) = { }})
    def hide() = System.setOut(dummy)
    def show() = System.setOut(originalStream)
}