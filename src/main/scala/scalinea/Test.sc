import scalinea.grammar._
import scalinea.grammar.ToLpInteger._

val a: LpInteger = "a"
val b = "b".toLpInteger


//val expr1 = 500*a + 400*b
val expr1: LpExpression = 300
val expr1bis = 200 - expr1
val expr2 = 300 + 400 + a + 200 + a
val expr3 = b + 400
val expr4 = a * 300 + 5
val expr5 = -(a * 200 + b)
val expr6 = 200 * a
val expr7 = 200 - a
val expr8 = -a
val expr9 = 3 + -(-a)

