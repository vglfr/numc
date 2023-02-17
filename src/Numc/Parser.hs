module Numc.Parser where

import Control.Applicative ((<|>), Alternative (many))
import Text.Trifecta (
    Parser, Result
  , alphaNum, between, chainl1, char, eof, integerOrDouble, lower, parens, parseString, symbol, token, try, whiteSpace
  )

import Numc.AST (Expr ((:+), (:-), (:*), (:/), Val, Var))

parseExpr :: String -> Result Expr
parseExpr = parseString (whiteSpace *> parseBin <* eof) mempty

parseBin :: Parser Expr
parseBin = expr
 where
  expr = chainl1 term addop
  term = chainl1 fact mulop
  fact = parens expr <|> ignoreSpace (try parseVal <|> parseVar)
  addop = (:+) <$ symbol "+"
      <|> (:-) <$ symbol "-"
  mulop = (:*) <$ symbol "*"
      <|> (:/) <$ symbol "/"
  ignoreSpace = between whiteSpace whiteSpace

parseVar :: Parser Expr
parseVar = Var <$> var
 where
  var = parens var <|> token (start <> letter)
  start = pure <$> lower
  letter = many $ alphaNum <|> char '_' <|> char '\''

parseVal :: Parser Expr
parseVal = Val . either fromInteger id <$> val
 where
  val = parens val <|> integerOrDouble
