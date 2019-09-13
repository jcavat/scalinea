package ch.hepia.scalinea
package solver

import ch.hepia.scalinea.format.{Failure, LPFormat, Output, Success}

object CbcLpSolver extends Solver {

  import scala.io.Source
  import sys.process._

  def solve(system: clause.System): Output[LPResult] = {

    system.exportTo(LPFormat) match {
      case Success(result, _) => {
        SolverUtil.writeLpFile(result.map(_ + "\n"), "test.lp")
      }
      case Failure(_, _) => println("Oups")
    }

    "cbc test.lp solve solu sol.txt".!
    val bufferedSol = Source.fromFile("sol.txt")
    val lines: Iterator[String] = bufferedSol.getLines()

    val statusLine = lines.nextOption().map( _.split(" ")(0) ).getOrElse("")
    val status = statusLine == "Optimal"

    var res: List[(String, String)] = List()

    for(line <- lines) {
      val cols = line.strip().split(" ").filter( !_.isBlank )
      res +:= cols(1)->cols(2)
    }

    bufferedSol.close()

    Success( MapSolution(status, res.toMap), Nil )

  }

}
