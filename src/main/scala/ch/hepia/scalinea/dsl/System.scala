package ch.hepia.scalinea
package dsl

import Ops._
import ch.hepia.scalinea.clause.Terms
import ch.hepia.scalinea.format.{Output, Success}
import ch.hepia.scalinea.solver._


object System {

  def define = SysState.empty

  sealed trait Constr
  sealed trait NoConstr extends Constr
  sealed trait HasConstr extends Constr
  sealed trait Goal
  sealed trait NoGoal extends Goal
  sealed trait HasGoal extends Goal

  sealed trait GoalTerms
  case class Minimize(terms: Terms) extends GoalTerms
  case class Maximize(terms: Terms) extends GoalTerms

  case class SysState[C<:Constr,G<:Goal] private(
    constr: List[dsl.Constr],
    gopt: Option[GoalTerms]
  ) {

    def constraints( cs: dsl.Constr* ): SysState[HasConstr,G] = {
      copy( constr = constr ++ cs.toList )
    }
    def constraints( cs: Iterable[dsl.Constr] ): SysState[HasConstr,G] = constraints( cs.toSeq: _* )

    def minimize( expr: dsl.Expr )( implicit ev: G =:= NoGoal ): SysState[C,HasGoal] = {
      require( ev != null ) //Always true in order to remove warning
      copy(gopt=Some(Minimize(expr.toTerms)))
    }

    def maximize( expr: dsl.Expr )( implicit ev: G =:= NoGoal ): SysState[C,HasGoal] = {
      require( ev != null ) //Always true in order to remove warning
      minimize( Mult( Const(-1), expr ) )
    }

    def build( implicit ev0: C =:= HasConstr, ev1: G =:= HasGoal ): clause.System = {
      require( ev0 != null && ev1 != null ) //Always true in order to remove warning
      val clauses = constr.map(_.toClause)
      val vars: List[clause.Var] = for {
        clause <- clauses
        sortedVars <- clause.terms.sortedVars
        v <- sortedVars.sortedVar
      } yield v
      clause.System( clauses, gopt.get, vars.toSet )
    }
  }
  object SysState {
    def empty: SysState[NoConstr,NoGoal] = new SysState(Nil,None)
  }

}


object SysDemo extends App {


  def showFmt( sys: clause.System ): Unit = {
    val output: Output[Iterable[String]] = format.LPFormat( sys )
    output match {
      case format.Success(results, _) => results.foreach( println )
      case err => println( "ERROR: " + err )
    }
  }


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

  val system: clause.System = {
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

  showFmt( system )





  /*
   * TODO: Check if max column in lp file
   * TODO: LP Format seems do not love `<` and `>`, only `<=` and `>=`
   * TODO: Use var outside the system to get the solution
   */
}
