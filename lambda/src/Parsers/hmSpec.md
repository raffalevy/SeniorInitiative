# AST:

```haskell
data Expr = 
    Var Name
    | Expr :$ Expr
    | Lam Name Expr
    | Let Name Expr Expr
```

# Grammar:

```
expression := letexpr | term+

term :=
| lambda name+ "." expression
| name
| "(" + expression + ")"

letexpr := "let" + name + "=" + expression + "in" + expression

lambda := "\" | "λ"

name := /[A-Za-z0-9]*/
```