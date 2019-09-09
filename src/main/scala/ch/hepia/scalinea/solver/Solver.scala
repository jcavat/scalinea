package ch.hepia.scalinea
package solver

import ch.hepia.scalinea.format.{Failure, LPFormat, Success}

trait Solver {
  def solve( system: clause.System ): Unit
}

object SolverUtil {
  def writeLpFile(lines: Iterable[String], filename: String) = {
    import java.io.File
    import java.io.PrintWriter

    val writer = new PrintWriter(new File(filename))
    lines.foreach(writer.write)
    writer.close()
  }
}


object FakeLpSolver extends Solver {

  def solve(system: clause.System): Unit = {

    system.exportTo(LPFormat) match {
      case Success(result, _) => {
        SolverUtil.writeLpFile(result.map(_ + "\n"), "test.lp")
      }
      case Failure(_, _) => println("Oups")
    }

  }

}
