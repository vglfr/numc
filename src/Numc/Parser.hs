module Numc.Parser where

import Prelude hiding (exponent)

import Control.Applicative ((<|>))
import Text.Trifecta (Parser, Result, chainl1, eof, integerOrDouble, parens, parseString, symbol)

import Numc.AST (Expr (Val, (:+), (:-), (:*), (:/)))

parseExpr :: String -> Result Expr
parseExpr = parseString (parseBin <* eof) mempty

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
