package ch.hepia
package scalinea
package solver

import ch.hepia.scalinea.dsl.{BVar, IVar, Var}
import ch.hepia.scalinea.format.{Failure, LPFormat, Output, Success}

trait Solution {
  def apply(v: BVar): Boolean
  def apply(v: IVar): Int
  def apply(v: Var): Double
  def status: LpStatus
}

sealed trait LpStatus
case object Optimal extends LpStatus
case object Infeasible extends LpStatus
case object Unbounded extends LpStatus
case object SubOptimal extends LpStatus
case object NotSolved extends LpStatus

case class MapSolution(status: LpStatus, sol: Map[String, String]) extends Solution {
  def apply(v: BVar): Boolean = {
    val res = sol(v.symbol)
    if (res == "0") {
      true
    } else if (res == "1") {
      false
    } else {
      throw new Exception("SHIT")
    }
  }
  def apply(v: IVar): Int = {
    sol(v.symbol).toInt
  }
  def apply(v: Var): Double = {
    sol(v.symbol).toDouble
  }
}

trait Solver {
  def solve( system: clause.System ): Output[Solution]
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

  import scala.io.Source
  import sys.process._

  def solve(system: clause.System): Output[Solution] = {

    system.exportTo(LPFormat) match {
      case Success(result, _) => {
        SolverUtil.writeLpFile(result.map(_ + "\n"), "test.lp")
      }
      case Failure(_, _) => println("Oups")
    }

    "cbc test.lp solve solu sol.txt && cat sol.txt".!
    val bufferedSol = Source.fromFile("sol.txt")
    val lines: Iterator[String] = bufferedSol.getLines()

    val statusLine = lines.nextOption().map( _.split(" ")(0) ).getOrElse("")
    val lpStatus = if(statusLine == "Optimal") Optimal else NotSolved

    var res: List[(String, String)] = List()

    for(line <- lines) {
      val cols = line.strip().split(" ").filter( !_.isBlank )
      res +:= ((cols(1), cols(2)): (String, String))
    }

    bufferedSol.close()

    Success( MapSolution(lpStatus, res.toMap), Nil )

  }

}
