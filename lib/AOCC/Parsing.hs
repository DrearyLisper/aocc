{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AOCC.Parsing where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser (\input -> [(f v, out) | (v, out) <- parse p input])

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = Parser (\input -> [(v, input)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = Parser (\input -> concat [parse (fmap f px) out | (f, out) <- parse pf input])

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser (\input -> concat [parse (f v) out | (v, out) <- parse p input])

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const [])

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q =
    Parser
      ( \input -> case parse p input of
          [] -> parse q input
          r -> r
      )

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

item :: Parser Char
item =
  Parser
    ( \case
        [] -> []
        (x : xs) -> [(x, xs)]
    )

three :: Parser (Char, Char)
three = do
  x <- item
  _ <- item
  z <- item
  return (x, z)

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

char :: Char -> Parser Char
char x = sat (x ==)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  _ <- char x
  _ <- string xs
  return (x : xs)

nat :: Parser Integer
nat = do
  xs <- some digit
  return (read xs)

int :: Parser Integer
int = (char '-' >> nat >>= \n -> return (-n)) <|> nat

space :: Parser ()
space = void (many (sat isSpace))

token :: Parser a -> Parser a
token p = do
  _ <- space
  v <- p
  _ <- space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Integer
natural = token nat

integer :: Parser Integer
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

list :: String -> String -> String -> Parser a -> Parser [a]
list openP closeP sep p =
  ( do
      _ <- symbol openP
      x <- p
      xs <- many (symbol sep >> p)
      _ <- symbol closeP
      return (x : xs)
  )
    <|> ( do
            _ <- symbol openP
            _ <- symbol closeP
            return []
        )