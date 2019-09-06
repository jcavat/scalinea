# LP-FORMAT

- Specs
  - http://lpsolve.sourceforge.net/5.1/CPLEX-format.htm
  - http://www.gurobi.com/documentation/8.0/refman/lp_format.html

## Requirements

- Variable names:
  - No spaces inside a variable name !
  - Max 16 chars  (lpsolve) or 255 (gurobi)
  - Alphanum, case sensitive, and ! " # $ % & ( ) / , . ; ? @ _ ` ' {
    } | ~
  - cannot begin with a number or a period
  - avoid 'e' or 'E' as first char or alone
