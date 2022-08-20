module SystemParser ( Term(..), Equation(..), sign, coefficient, term, equation ) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Term = Term { _coefficient:: Int, _unknown:: Char } deriving Show
data Equation = Equation { _terms:: [Term], _value:: Int } deriving Show

sign :: Parser Char
sign = char '+' <|> char '-'

coefficient :: Parser Int
coefficient = do
  s <- sign
  c <- some digitChar
  return $ case s of '+' -> (read c)::Int
                     _   -> ((read c)::Int) * (-1)

term :: Parser Term
term = do
  c <- coefficient
  u <- lowerChar
  return $ Term c u

equation :: Parser Equation
equation  = do
  t <- some term
  _ <- char '='
  v <- digitChar
  _ <- char ';'
  return $ Equation t ((read [v])::Int)
