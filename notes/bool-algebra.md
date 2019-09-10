# Rules for Binary/boolean vars

| Logic domain            | Linear constraint domain    |
| ----------------------- | --------------------------- |
| `x` is true | `x = 1`  |
| `x` is false | `x = 0`  |
| `x -> y` | `x <= y`  |
| `x <-> y` (eq to `x -> y and y -> x`) | `x = y` (eq to `x <= y, y <= x`)  |
| `!x -> y` | `(1-x) <= y`  |
| `x or y` | `x + y >= 1`  |
| `x or y -> z` | `x + y <= 2*z`  |
| `x or ... or xn -> z` | `x + ... + xn <= n*z`  |
| `x and y` | `x + y = 2`  |
| `x and y -> z` | `x + y -1 <= z`  |
| `x1 and x2 and x3 -> z` | `x1 + x2 + x3 - 2 <= z`  |
| `x1 and ... and xn -> z` | `x1 + x2 + x3 - (n-1) <= z`  |
| `x -> y1 or y2 or y3` | `x <= y1 + y2 + y3`  |
| `x -> y1 and y2 and y3` | `3*x <= y1 + y2 + y3`  |

## Simplification

we can replace the `imply`

- `a -> b` is equivalent to `!a or b` = `(1-a) + b >= 1`
  -  `y1 and ... and y3 -> x1 or x2 or x3` 
  - is equivalent to `!(y1 and ... and yn) or (x1 or ... or xn)`
  - `!y1 or ... or !yn or x1 or ... or xn`
  - `(1-y1) + ... + (1-yn) + x1 + ... + xn >= 1`

## Conjunctive normal form (CNF)

All propositional formulae can be converted into an equivalent formula in conjunctive normal form (see [wikipedia](https://en.wikipedia.org/wiki/Conjunctive_normal_form))

a CNF is a conjunction of disjunction clauses under the form `(X1 or X2 or X3) and (X4 or X5) and X6 and ... and (Xn-1 or Xn)`

### Example

`(x1 and y1) and (x2 and y2) and ... and (xn and yn)`

can be transformed to

`(x1 or x2 or ... or xn) and (y1 or x2 or ... or xn) and (x1 or y2 or ... or xn) and ... and (y1 or y2 or ... or yn)`

which contains 2^i clauses.

By introducing new variables we avoid exponential increase :

`(z1 or ... or zn) and (!z1 or x1) and (!z1 or y1) and ... and (!zn or xn) and (!zn or yn)`

An alternative, is to replace each and-clause by a new var :

`z1 and z2 and ... and zn`

with: `z1 = x1 and y1`, `z2 = x2 and y2`, `...`, `zn = xn and yn`


