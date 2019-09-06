package ch.hepia.scalinea
package solver

import ch.hepia.scalinea.format.{Failure, LPFormat, Success}

trait Solver {
  def solve( system: clause.System ): Unit
}


object FakeLpSolver extends Solver {
  import java.io.File
  import java.io.PrintWriter

  override def solve(system: clause.System): Unit = {

    val writer = new PrintWriter(new File("test.lp"))

    system.exportTo(LPFormat) match {
      case Success(result,_) => result.map(_ + "\n").foreach( writer.write )
      case Failure(_,_) => println("Oups")
    }

    writer.close()

  }
}
