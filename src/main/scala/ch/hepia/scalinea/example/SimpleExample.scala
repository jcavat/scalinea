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
  val joel = BVar("joel")
  val jl = BVar("jl")
  val paul = BVar("paul")
  val seb = BVar("seb")

  val system = {
    dsl.System.define.constraints(
      //paul | (!paul | paul)

      //(joel & !jl & !paul) | (jl & !joel & !paul) | paul
      //
      joel | !joel `imply` exactlyOneOf(joel, jl, paul, seb)


    ).maximize(
      joel
    ).build
  }

  val solver: Solver = CbcLpSolver // or GurobiSolver
  solver.solve(system) match {
    case Success(sol: Solution, _) if sol.isOptimal => {
      println("Optimal")
      /*
      for( v <- Seq(a,b,c) ) {
        println( s"${v.symbol}: ${sol(v)}" )
      }

       */
      for( v <- Seq(joel, jl, paul, seb) ) {
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
