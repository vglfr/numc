module Numc.Parser where

import Prelude hiding (exponent)

import Control.Applicative ((<|>))
import Text.Trifecta (Parser, chainl1, integerOrDouble, parens, symbol)

import Numc.AST (Expr (Val, (:+), (:-), (:*), (:/)))

parseExpr :: Parser Expr
parseExpr = parseBin

parseBin :: Parser Expr
parseBin = expr
 where
  expr = chainl1 term addop
  term = chainl1 val mulop
  val = parens expr <|> parseVal
  addop = (:+) <$ symbol "+"
      <|> (:-) <$ symbol "-"
  mulop = (:*) <$ symbol "*"
      <|> (:/) <$ symbol "/"

parseVal :: Parser Expr
parseVal = Val . either fromInteger id <$> val
 where
  val = parens val <|> integerOrDouble
