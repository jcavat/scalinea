package ch.hepia.scalinea
package dsl

abstract class DSL1 {

  private var clauses: List[clause.Clause] = Nil
  private var goal: Option[clause.Terms] = None

  protected def addConstraint( constr: dsl.Constr ): Unit = {
    clauses = constr.toClause :: clauses
  }

  protected def setGoal( expr: dsl.Expr ): Unit = {
    if( ! goal.isDefined ) goal = Some(expr.toTerms)
    else throw new IllegalStateException("Goal function already defined")
  }

  def build: clause.System = clause.System( clauses, goal.get )

}

object DSL2 {

  class System {
    private var clauses: List[clause.Clause] = Nil
    private var gopt: Option[clause.Terms] = None

    def :=( constr: dsl.Constr ): Unit = {
      clauses = constr.toClause :: clauses
    }
    def setGoal( expr: dsl.Expr ): Unit = {
      if( ! gopt.isDefined ) gopt = Some(expr.toTerms)
      else throw new IllegalStateException("Goal function already defined")
    }
    def build: clause.System = clause.System( clauses, gopt.get )
  }

  def apply( f: System=>Unit ): clause.System = {
    val s = new System
    f(s)
    s.build
  }

}

object DSL3 {

  def define = SysState.empty

  sealed trait Constr
  sealed trait NoConstr extends Constr
  sealed trait HasConstr extends Constr
  sealed trait Goal
  sealed trait NoGoal extends Goal
  sealed trait HasGoal extends Goal

  class SysState[C<:Constr,G<:Goal] private(
    private val clauses: List[clause.Clause],
    private val gopt: Option[clause.Terms]
  ) {
    def constraints( cs: dsl.Constr* ): SysState[HasConstr,G] = {
      val clauses2 = cs.toList.map( _.toClause )
      new SysState( clauses ++ clauses2 , gopt )
    }
    def goal( expr: dsl.Expr )( implicit ev: G =:= NoGoal ): SysState[C,HasGoal] = {
      require( ev != null ) //Always true in order to remove warning
      new SysState(clauses,Some(expr.toTerms))
    }
    def build( implicit ev0: C =:= HasConstr, ev1: G =:= HasGoal ): clause.System = {
      require( ev0 != null && ev1 != null ) //Always true in order to remove warning
      clause.System( clauses, gopt.get )
    }
  }
  object SysState {
    def empty: SysState[NoConstr,NoGoal] = new SysState(Nil,None)
  }


}


object SysDemo extends App {

  import Ops._

  def showFmt( sys: clause.System ): Unit = {
    val output = format.LPFormat( sys )
    output match {
      case format.Success(results, _) => results.foreach( println )
      case err => println( "ERROR: " + err )
    }
  }

  /**** DSL 1 ****/

  val sys1 = new DSL1 {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    addConstraint( 3*x + y < 2*z )
    addConstraint( -x < y )
    addConstraint( x + y + z >= 0 )

    setGoal( x - z )
  }.build

  showFmt( sys1 )
  println

  /**** DSL 2 ****/

  val sys2 = DSL2 { s =>
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    s :=  3*x + y < 2*z
    s :=  -x < y 
    s := x + y + z >= 0

    s.setGoal(x - z)
  }

  showFmt( sys2 )

  println


  /**** DSL 3 ****/

  val sys3 = {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    DSL3.define.constraints(
      3*x + y < 2*z,
      -x < y,
      x + y + z >= 0,
    ).goal( x - z )
    .build
  }

  showFmt( sys3 )


}
