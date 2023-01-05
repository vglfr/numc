module Numc.Parser where

import Prelude hiding (exponent)

import Data.Functor ((<&>))
import Text.Parsec (Parsec, char, choice, digit, many, many1, oneOf, string, try, (<|>))

import Numc.AST (Expr (Val))
-- import Numc.AST (Expr (Val, (:+), (:-), (:*), (:/)))

parseExpr :: Parsec String () Expr
parseExpr = try parseBin <|> try parseVal

{-
[+-]?\d+                    -- integer-like
[+-]?\d*\.\d*([+-]?[eE]d+)? -- float-like
-}

parseVal :: Parsec String () Expr
parseVal = Val . read <$> (sign <> number)
 where
  sign = try (string "-") <|> try (char '+' *> mempty) <|> mempty
  number = try float <|> int
  float = choice [leftFloat, rightFloat] <> choice [exponent, mempty]
  leftFloat = int <> string "." <> digitOrZero
  rightFloat = digitOrZero <> string "." <> int
  exponent = (pure <$> oneOf "eE") <> sign <> many1 digit
  digitOrZero = many digit <&> \x -> if null x then "0" else x
  int = many1 digit

parseBin :: Parsec String () Expr
parseBin = undefined
