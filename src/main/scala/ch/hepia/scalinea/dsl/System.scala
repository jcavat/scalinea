package ch.hepia.scalinea
package dsl

import Ops._
import ch.hepia.scalinea.clause.Terms
import ch.hepia.scalinea.format.{Output, Success}
import ch.hepia.scalinea.solver.{FakeLpSolver, Solution, Solver}


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





  val x = Var("x").minBound(0).maxBound(10)
  val y = Var("y").range(0,20.5)
  val z = Var("z").maxBound(40).minBound(Double.NegativeInfinity)
  val t = Var("t").free
  val i = IVar("i").range(1, 5)
  val b = BVar("b")

  val system: clause.System = {


    dsl.System.define.constraints(
      x >= 1,
      y >= 10,
      i + t <= 30,
      b <= i,
      i >= 2* x
    ).constraints(
      z <= y,
      t <= 11.4
    ).maximize(
      //x + y + z + t + i + b + List(x,y,z).foldRight(Const(0): Expr)(_ + _)

      x + y + z + t + i + b + sum(x,y,z) + sum(List(x,y,z))

    ).build
  }

  val solver: Solver = FakeLpSolver
  val res: Output[Solution] = solver.solve(system)
  res match {
    case Success(sol: Solution, _) => {
      println("*" * 10 + "\n" + sol.isOptimal )
      for( v <- List(x,y,z,t) ) {
        println( s"${v.symbol}: ${sol(v)}" )
      }
      println( s"${i.symbol}: ${sol(i)}" )
      println( s"${b.symbol}: ${sol(b)}" )

    }
    case _ => println("oups")
  }
 
  showFmt( system )





  /*
   * TODO: Check if max column in lp file
   * TODO: LP Format seems do not love `<` and `>`, only `<=` and `>=`
   * TODO: Use var outside the system to get the solution
   */

  /*
  trait AVar {
    type T
  }
  trait Solution {
    def apply( v: AVar ): v.T
  }
  */
}
