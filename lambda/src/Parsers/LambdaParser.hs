-- Parser for untyped lambda calculus.

module Parsers.LambdaParser where

import Control.Applicative
import Control.Monad
import Data.Char

import Untyped

newtype Parser t = Parser { runParser :: String -> Maybe (t, String) }

exact :: Char -> Parser Char
exact c = Parser $ \s -> case s of
    [] -> Nothing
    x:xs -> if c == x then Just (c, xs) else Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Just ((), [])
    _ -> Nothing

instance Functor Parser where
    fmap f p = Parser $ \s -> case runParser p s of
        Just (x, s) -> Just (f x, s)
        Nothing -> Nothing

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    liftA2 f a b = Parser $ \s -> case runParser a s of
        Just (x, s) -> case runParser b s of
            Just (y, s) -> Just (f x y, s)
            Nothing -> Nothing
        Nothing -> Nothing

opt :: Parser t -> Parser (Maybe t)
opt p = Parser $ \s -> case runParser p s of
    Just (x, s) -> Just (Just x, s)
    Nothing -> Just (Nothing, s)

instance Alternative Parser where
    empty = Parser $ \s -> Nothing
    a <|> b = Parser $ \s -> case runParser a s of
        Just (x, s) -> Just (x, s)
        Nothing -> case runParser b s of
            Just (x, s) -> Just (x, s)
            Nothing -> Nothing

matches :: (Char -> Bool) -> Parser Char
matches pred = Parser $ \s -> case s of
    [] -> Nothing
    x:xs -> if pred x then Just (x, xs) else Nothing

alpha :: Parser Char
alpha = matches isAlpha

alphaNum :: Parser Char
alphaNum = matches isAlphaNum

space :: Parser Char
space = matches isSpace

whiteSpace :: Parser ()
whiteSpace = void $ many space

token :: Parser t -> Parser t
token p = whiteSpace *> p <* whiteSpace

lambda :: Parser ()
lambda = token $ void $ exact '\955' <|> exact '\\'

name :: Parser String
name = token $ some alphaNum

openParen :: Parser ()
openParen = token $ void $ exact '('

closeParen :: Parser ()
closeParen = token $ void $ exact ')'

dot :: Parser ()
dot = token $ void $ exact '.'

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