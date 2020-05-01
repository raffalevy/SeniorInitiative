# Typed Lambda Calculus Implementations in Haskell

- `src/Untyped.hs` is an interpreter for the **untyped lambda calculus**.

- `src/STLCSimple.hs` is a **type-checker for the simply typed lambda calculus**, given explicit type annotations.

- `src/STLCInference.hs` implements **type inference for the simply typed lambda calculus**. It does not require explicit type annotations.

- `src/HindleyMilner.hs` implements **Hindley-Milner type inference**; in addition to simply-typed functions, it can infer polymorphic types such as `forall T, T -> T` or `forall A B, A -> B -> pair[A,B]`.

- `src/REPL.hs` is a REPL **front-end interface** for Hindley-Milner type inference; the user can enter terms and their type will be displayed.

- `src/Parsers/` contains parsers which **convert text into lambda expressions**.