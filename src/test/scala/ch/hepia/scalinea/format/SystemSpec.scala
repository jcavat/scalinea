package ch.hepia.scalinea.format

import ch.hepia.scalinea.clause.System
import ch.hepia.scalinea.dsl
import ch.hepia.scalinea.dsl.Var
import org.scalatest.{FlatSpec, Matchers}

class LPFormatSpec extends FlatSpec with Matchers {

  "A linear and quadratic DSL System" should "map to LP Format" in {

    val x1 = Var("x1")
    val x2 = Var("x2").minBound(2.0)
    val x3 = Var("x3").maxBound(3.0)

    val system: System = {
      import ch.hepia.scalinea.dsl.Ops._
      dsl.System.define.constraints(
        x1 <= 10.0,
        x1 + x2 <= 20.0,
        2*x3 <= 2.0,
        x3 * x3 >= 0.0
      ).maximize(
        x1 + x2 + x3
      ).build
    }

    LPFormat.goalLpSection(system) should be ("Minimize\n obj: -1.0 x1 + -1.0 x2 + -1.0 x3")
    LPFormat.constraintsLpSection(system) should be (List(
      "Subject To",
      " c0: 1.0 x1 <= 10.0",
      " c1: 1.0 x1 + 1.0 x2 <= 20.0",
      " c2: 2.0 x3 <= 2.0",
      " qc3: [ 1.0 x3 ^ 2 ] >= 0")
    )

  }

}
