package ch.hepia
package scalinea
package solver

import ch.hepia.scalinea.dsl.{BVar, IVar, Var}
import ch.hepia.scalinea.format.Output

sealed trait LPResult
trait Solution  extends LPResult {
  def apply(v: BVar): Boolean
  def apply(v: IVar): Int
  def apply(v: Var): Double
  def isOptimal: Boolean
}

case object Infeasible extends LPResult
case object Unbounded extends LPResult
case object NotSolved extends LPResult

case class MapSolution(isOptimal: Boolean, sol: Map[String, String]) extends Solution {
  def apply(v: BVar): Boolean = {
    val res: Option[String] = sol.get(v.symbol)
    if(res.isEmpty) {
      return false
    }
    if (res.contains("0")) {
      false
    } else if (res.contains("1")) {
      true
    } else {
      throw new Exception("SHIT")
    }
  }
  def apply(v: IVar): Int = {
    sol(v.symbol).toIntOption.getOrElse(0)
  }
  def apply(v: Var): Double = {
    sol(v.symbol).toDoubleOption.getOrElse(0.0)
  }
}

trait Solver {
  def solve( system: clause.System ): Output[LPResult]
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



