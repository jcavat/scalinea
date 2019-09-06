package ch.hepia.scalinea
package dsl

import ch.hepia.scalinea.clause.Terms
import ch.hepia.scalinea.format.Output
import ch.hepia.scalinea.solver.{FakeLpSolver, Solver}


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

  val system: clause.System = {
    import Ops._

    val x = Var("x").minBound(0).maxBound(10)
    val y = Var("y").range(0,20.5)
    val z = Var("z").maxBound(40)
    val t = Var("t").free
    //val u = Var("u")

    dsl.System.define.constraints(
      x >= 1,
      y >= 10
    ).constraints(
      z <= y,
      t <= 11.4
    ).maximize(
      x + y + z + t
    ).build
  }

  val solver: Solver = FakeLpSolver
  solver.solve(system)
 
  showFmt( system )


}
