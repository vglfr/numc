module Numc.Parser where

import Prelude hiding (exponent)

import Control.Applicative ((<|>))
import Text.Trifecta (Parser, integerOrDouble, noneOf, notFollowedBy, oneOf, parens, token, try)

import Numc.AST (Expr (Val, (:+), (:-), (:*), (:/)))

parseExpr :: Parser Expr
parseExpr = try parseBin <|> parseVal

parseVal :: Parser Expr
parseVal = Val . either fromInteger id <$> val
 where
  val = parens val <|> integerOrDouble

parseBin :: Parser Expr
parseBin = outers bin
 where
  bin = do
    a <- try (parens bin) <|> parseVal
    o <- token $ oneOf "+-*/"
    b <- try (parens bin) <|> parseVal
    pure $ case o of
             '+' -> a :+ b
             '-' -> a :- b
             '*' -> a :* b
             '/' -> a :/ b
             _   -> error "absurd"

outers :: Parser a -> Parser a
outers p = try (parens (outers p) <* notFollowedBy (noneOf ")")) <|> p
