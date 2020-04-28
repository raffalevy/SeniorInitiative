# AST:

```haskell
data Expr = 
    Var Name
    | App Expr Expr
    | Lam Name Expr deriving (Show)
```

# Grammar:

```
expression := term+

term :=
| lambda name+ "." expression
| name
| "(" + expression + ")"

lambda := "\" | "λ"

name := /[A-Za-z0-9]*/
```