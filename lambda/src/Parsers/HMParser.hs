module Parsers.HMParser where

import Control.Applicative
import Control.Monad
import Data.Char

import HindleyMilner
import Parsers.Combinators hiding (name)

expression :: Parser Expr
expression =
    letExpr
    <|> (foldl1 App <$> some term)

term :: Parser Expr
term =
    lambdaTerm
    <|> (Var <$> name)
    <|> openParen *> expression <* closeParen

lambdaTerm :: Parser Expr
lambdaTerm = (\params body -> foldr Lam body params) <$>
    (lambda *> some name <* dot) <*> expression

letExpr :: Parser Expr
letExpr = Let <$>
    (letKW *> name <* equal) <*> (expression <* inKW) <*> expression

word :: Parser String
word = token $ some alphaNum

name :: Parser String
name = failOn anyKeyword *> word

keyword :: String -> Parser String
keyword k = token $ exacts k

anyKeyword :: Parser String
anyKeyword = letKW <|> inKW

letKW :: Parser String
letKW = keyword "let"

inKW :: Parser String
inKW = keyword "in"

equal :: Parser Char
equal = token $ exact '='

parse :: String -> Maybe Expr
parse s = fst <$> runParser (expression <* eof) s