package ch.hepia.scalinea
package solver

import ch.hepia.scalinea.format.{Failure, LPFormat, Output, Success}

object GurobiSolver extends Solver {

  import scala.io.Source
  import sys.process._

  def solve(system: clause.System): Output[LPResult] = {

    system.exportTo(LPFormat) match {
      case Success(result, _) => {
        SolverUtil.writeLpFile(result.map(_ + "\n"), "test.lp")
      }
      case Failure(_, _) => println("Oups")
    }

    val log = "gurobi_cl ResultFile=sol.sol test.lp".!!

    val bufferedSol = Source.fromFile("sol.sol")
    val lines: Iterator[String] = bufferedSol.getLines()

    var res: List[(String, String)] = List()

    for(line <- lines.filter( !_.startsWith("#"))) {
      val cols = line.strip().split(" ")
      res +:= ((cols(0), cols(1)): (String, String))
    }

    bufferedSol.close()

    Success( MapSolution( log.contains("Optimal solution found") , res.toMap), Nil )

  }

}
