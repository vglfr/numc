module Numc.Parser where

import Prelude hiding (exponent)

import Control.Applicative ((<|>))
import Text.Trifecta (Parser, integerOrDouble, oneOf, parens, whiteSpace)

import Numc.AST (Expr (Val, (:+), (:-), (:*), (:/)))

parseExpr :: Parser Expr
parseExpr = parseBin <|> parseVal

parseVal :: Parser Expr
parseVal = Val . either fromInteger id <$> integerOrDouble

parseBin :: Parser Expr
parseBin = parens bin <|> bin
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
