module Numc.Parser where

import Prelude hiding (exponent)

import Control.Applicative ((<|>))
import Text.Trifecta (Parser, integerOrDouble, oneOf, parens, try, whiteSpace)

import Numc.AST (Expr (Val, (:+), (:-), (:*), (:/)))

parseExpr :: Parser Expr
parseExpr = expr -- parens expr <|> expr
 where
  expr = try parseBin <|> parseVal

parseVal :: Parser Expr
parseVal = Val . either fromInteger id <$> val
 where
  val = parens val <|> integerOrDouble

parseBin :: Parser Expr
parseBin = try (parens parseBin) <|> bin
 where
  bin = do
    a <- parseVal
    o <- oneOf "+-*/" <* whiteSpace
    b <- parseVal
    pure $ case o of
             '+' -> a :+ b
             '-' -> a :- b
             '*' -> a :* b
             '/' -> a :/ b
             _   -> error "absurd"
