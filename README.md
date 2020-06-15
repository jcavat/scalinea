# Scalinea
[![Build Status](https://travis-ci.com/jcavat/scalinea.svg?branch=master)](https://travis-ci.com/jcavat/scalinea)
[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

## Summary

This project provides a mathematical programming modeling library. It aims to provide a DSL for the creation 
of linear and quadratic optimization model. Solution will be solve with CBC, lp_solve, glpk and Gurobi.

## Current version

- Basic DSL to create a model
- Solution provided by CBC-Coin Or and Gurobi

This current version is usable but is subject to change

## Example

```scala
val a = IVar("a")
val b = IVar("b")
val c = IVar("c")

val system = {
  dsl.System.define.constraints(
    500*a + 1200*b + 1500*c <= 10000,
    a <= b
  ).maximize(
    10.0*a + 20.0*b
  ).build
}

val solver: Solver = CbcLpSolver // or GurobiSolver
solver.solve(system) match {
  case Success(sol: Solution, _) => {
    println("Optimal: " + sol.isOptimal )
    for( v <- Seq(a,b,c) ) {
      println( s"${v.symbol}: ${sol(v)}" )
    }
  }
  case _ => println("oups")
}
```

## News

DSL for binary variables is now possible

```scala
  val system = {
    dsl.System.define.constraints(
      x `imply` z,
      z `iif` w,
      z & w,
      x | y `imply` exactlyOneOf(z1, z2 | z3, z1 & z2, z4 `iif` z5)
      ...

    ).maximize(
      ...
    ).build
```

## Next step

- add new solvers (glpk, lp_solve, Cplex)
- improve existing solver solution parsing with explicit status (Unboundable, Unfeasible, ...)
- add more tests and more examples
- create documentation
- publish the first beta release :)




