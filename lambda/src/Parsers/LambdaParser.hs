-- Parser for untyped lambda calculus.

module Parsers.LambdaParser where

import Control.Applicative
import Control.Monad
import Data.Char

import Untyped
import Parsers.Combinators

expression :: Parser Expr
expression = foldl1 App <$> some term

term :: Parser Expr
term =
    lambdaTerm
    <|> (Var <$> name)
    <|> openParen *> expression <* closeParen

lambdaTerm :: Parser Expr
lambdaTerm = (\params body -> foldr Lam body params) <$>
    (lambda *> some name <* dot) <*> expression

parse :: String -> Maybe Expr
parse s = fst <$> runParser (expression <* eof) s