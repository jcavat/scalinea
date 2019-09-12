package ch.hepia.scalinea.format

import ch.hepia.scalinea.clause.System
import ch.hepia.scalinea.dsl
import ch.hepia.scalinea.dsl.{BVar, IVar, Var}
import org.scalatest.{FlatSpec, Matchers}

class LPFormatSpec extends FlatSpec with Matchers {

  def system: System = {
    val x1 = Var("x1")
    val x2 = Var("x2").minBound(2.0)
    val x3 = Var("x3").maxBound(3.0)
    val i = IVar("i").range(1, 4)
    val b = BVar("b")

    // This model is probably unfeasible
    val system: System = {
      import ch.hepia.scalinea.dsl.Ops._
      dsl.System.define.constraints(
        x1 <= 10.0,
        x1 + x2 <= 20.0,
        2*x3 <= 2.0,
        x3 * x3 >= 0.0,
        i <= 2 * x2,
        b <= i
      ).maximize(
        x1 + x2 + x3
      ).build
    }
    system
  }

  "A linear and quadratic DSL System" should "create the objective section" in {
    LPFormat.goalLpSection(system) should be("Minimize\n obj: -1.0 x1 + -1.0 x2 + -1.0 x3")
  }

  it should "create constraints section" in {
    LPFormat.constraintsLpSection(system) should be(List(
      "Subject To",
      " c0: 1.0 x1 <= 10.0",
      " c1: 1.0 x1 + 1.0 x2 <= 20.0",
      " c2: 2.0 x3 <= 2.0",
      " qc3: [ 1.0 x3 ^ 2 ] >= 0",
      " c4: 1.0 i + -2.0 x2 <= 0",
      " c5: 1.0 b + -1.0 i <= 0")
    )
  }

  it should "create a bound section" in {
    LPFormat.boundsLpSection(system).toSet should be(Set("Bounds", " 2.0 <= x2", " x3 <= 3.0", " 1 <= i <= 4"))
  }

  it should "create a general section" in {
    LPFormat.generalsLpSection(system) should be(List("General", " i"))
  }

  it should "create a binary section" in {
    LPFormat.binariesLpSection(system) should be(List("Binary", " b"))
  }

  it should "provide the overall format" in {
    LPFormat(system) match {
      case Success(result, _) => {
        result.mkString("\n") should be (
          """Minimize
            | obj: -1.0 x1 + -1.0 x2 + -1.0 x3
            |Subject To
            | c0: 1.0 x1 <= 10.0
            | c1: 1.0 x1 + 1.0 x2 <= 20.0
            | c2: 2.0 x3 <= 2.0
            | qc3: [ 1.0 x3 ^ 2 ] >= 0
            | c4: 1.0 i + -2.0 x2 <= 0
            | c5: 1.0 b + -1.0 i <= 0
            |Bounds
            | 2.0 <= x2
            | 1 <= i <= 4
            | x3 <= 3.0
            |General
            | i
            |Binary
            | b
            |End""".stripMargin)
      }
      case _ => fail()
    }
  }
}
