package ch.hepia.scalinea.example

import ch.hepia.scalinea.dsl.IVar
import ch.hepia.scalinea.format.Success
import ch.hepia.scalinea.solver.{CbcLpSolver, Solution, Solver}
import ch.hepia.scalinea.dsl
import ch.hepia.scalinea.dsl.Ops._

object SimpleExample extends App {

  val a = IVar("a")
  val b = IVar("b")
  val c = IVar("c")

  val system = {
    dsl.System.define.constraints(
      500*a + 1200*b + 1500*c <= 10000,
      a <= b
    ).maximize(
      10.0*a + 20.0*b
    ).build
  }

  val solver: Solver = CbcLpSolver // or GurobiSolver
  solver.solve(system) match {
    case Success(sol: Solution, _) => {
      println("Optimal: " + sol.isOptimal )
      for( v <- Seq(a,b,c) ) {
        println( s"${v.symbol}: ${sol(v)}" )
      }
    }
    case _ => println("oups")
  }
}
