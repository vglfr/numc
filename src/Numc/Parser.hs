module Numc.Parser where

import Control.Applicative ((<|>), Alternative (many))
import Text.Trifecta (
    Parser, Result
  , alphaNum, chainl1, char, eof, integerOrDouble, lower, parens, parseString, semiSep, symbol, token, try
  )

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Val, Var))

parse :: String -> Result [Expr]
parse = parseString parseFile mempty

parseFile :: Parser [Expr]
parseFile = semiSep (try parseAss <|> parseBin) <* eof

parseAss :: Parser Expr
parseAss = do
  v <- parseVar 
  _ <- token $ char '=' 
  e <- parseBin
  pure $ v := e

parseBin :: Parser Expr
parseBin = expr
 where
  expr = chainl1 term addop
  term = chainl1 fact mulop
  fact = parens expr <|> try parseVal <|> parseVar
  addop = (:+) <$ symbol "+"
      <|> (:-) <$ symbol "-"
  mulop = (:*) <$ symbol "*"
      <|> (:/) <$ symbol "/"

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




parseExpr :: String -> Result Expr
parseExpr = parseString (parseBin <* eof) mempty
