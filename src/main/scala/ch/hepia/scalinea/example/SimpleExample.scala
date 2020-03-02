package ch.hepia.scalinea.example

import ch.hepia.scalinea.dsl.{BExpr, BVar, IVar}
import ch.hepia.scalinea.format.Success
import ch.hepia.scalinea.solver.{CbcLpSolver, Solution, Solver}
import ch.hepia.scalinea.dsl
import ch.hepia.scalinea.dsl.Ops._

object SimpleExample extends App {

  val a = IVar("a")
  val b = IVar("b")
  val c = IVar("c")
  val b1 = BVar("joelwork")
  val b2 = BVar("jlwork")
  val b3 = BVar("paulwork")
  val b4 = BVar("sebwork")
  val test: Seq[BExpr] = List(b1, b2, b3, b4)

  val system = {
    dsl.System.define.constraints(
      500*a + 1200*b + 1500*c <= 10000,
      (b1 | b2) & !b3 `iif` b4,
      !b1,
      b4,
      a <= b
    ).maximize(
      10.0*a + 20.0*b + 3.0*b2
    ).build
  }

  val solver: Solver = CbcLpSolver // or GurobiSolver
  solver.solve(system) match {
    case Success(sol: Solution, _) => {
      println("Optimal: " + sol.isOptimal )
      for( v <- Seq(a,b,c) ) {
        println( s"${v.symbol}: ${sol(v)}" )
      }
      for( v <- Seq(b1, b2, b3, b4) ) {
        if( sol(v) ){
          println(s"${v.symbol} is true")
        } else {
          println(s"${v.symbol} is false")
        }
      }
    }
    case _ => println("oups")
  }
}
