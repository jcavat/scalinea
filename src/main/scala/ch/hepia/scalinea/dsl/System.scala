package ch.hepia.scalinea
package dsl

import ch.hepia.scalinea.clause.Terms


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
    val output = format.LPFormat( sys )
    output match {
      case format.Success(results, _) => results.foreach( println )
      case err => println( "ERROR: " + err )
    }
  }

  val system = {
    import Ops._

    val x = Var("x").minBound(0.3)
    val y = Var("y").range(0,20)
    val z = Var("z").maxBound(40)
    //val t = Var("t").free
    //val u = Var("u")

    System.define.constraints(
      x <= 1,
      y >= 10
    ).constraints(
      z <= y
    ).maximize(
      x + y + z
    ).build
  }
 
  showFmt( system )

  /*
   * Format[T] specify Output[T]
   * LPFormat[Iterable[String]] specify Output[Iterable[String]]
   * but nothing can constraint that a solver inheriting from Solver[Output[T]] can solve a real LP format, not
   * just Iterable[String]
   * should we create a type for Iterable[String] ? But what its name ? for sure something representing LpFormat
   * but "LpFormat" is already used
   */

}
