# Rules for Binary/boolean vars

| Logic domain            | Linear constraint domain    |
| ----------------------- | --------------------------- |
| `x` is true | `x = 1`  |
| `x` is false | `x = 0`  |
| `x -> y` | `x <= y`  |
| `x <-> y` (eq to `x -> y and y -> x`) | `x = y` (eq to `x <= y, y <= x`)  |
| `!x -> y` | `(1-x) <= y`  |
| `x or y` | `x + y >= 1`  |
| `x and y` | `x + y = 2`  |
| `x -> y1 or y2 or y3` | `x <= y1 + y2 + y3`  |
| `x -> y1 and y2 and y3` | `3*x <= y1 + y2 + y3`  |
| `y1 or y2 or y3 -> x` | `y1 + y2 + y3 <= 3*x`  |
| `y1 and y2 and y3 -> x` | `y1 + y2 + y3 - 2 <= x`  |

## Simplification

we can replace the `imply`

- `a -> b` is equivalent to `!a or b` = `(1-a) + b >= 1`

## Conjonctive normal form (CNF)

//TODO
