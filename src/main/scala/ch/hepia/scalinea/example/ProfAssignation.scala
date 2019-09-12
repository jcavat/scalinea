package ch.hepia.scalinea.example

import ch.hepia.scalinea.dsl.BVar
import ch.hepia.scalinea.format.{Output, Success}
import ch.hepia.scalinea.solver.{CbcLpSolver, LPResult, Solution, Solver}

import ch.hepia.scalinea.dsl
import ch.hepia.scalinea.dsl.Ops._

object ProfAssignation extends App {

  val profs = List("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12")
  val days = List("mo", "tu", "we", "th", "fr")
  val pref = Map(
    "p1" -> Map("mo" -> 3, "tu" -> 2),
    "p12" -> Map("th" -> 4, "fr" -> 1)
  )

  val vars = (for {
    p <- profs
    d <- days
  } yield BVar(s"${p}_$d")).map( v => v.symbol -> v ).toMap

  val system = {
    dsl.System.define.constraints(
      // it should have at least two professors per day
      for(d <- days) yield sum(vars.values.filter( _.symbol.endsWith(s"_$d") ) ) >= 2
    ).constraints(
      // a professor should work only one day
      for(p <- profs) yield sum(vars.values.filter( _.symbol.startsWith(s"${p}_") ) ) === 1
    ).constraints(
      // if p1 works on monday, p2 works on monday, too
      vars("p1_mo") <= vars("p2_mo")
    ).maximize(
      sum(for {
        p <- profs
        d <- days
        if pref.get(p).flatMap(prefProf => prefProf.get(d) ).isDefined
      } yield pref(p)(d) * vars(s"${p}_$d")
      )
    ).build
  }

  val solver: Solver = CbcLpSolver
  val res: Output[LPResult] = solver.solve(system)
  res match {
    case Success(sol: Solution, _) => {
      println("Optimal: " + sol.isOptimal )
      for( v <- vars.values ) {
        if (sol(v))
          println( s"${v.symbol}: ${sol(v)}" )
      }
    }
    case _ => println("oups")
  }
}
