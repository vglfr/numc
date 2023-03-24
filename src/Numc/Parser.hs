module Numc.Parser where

import Control.Applicative ((<|>), Alternative (many))
import Text.Trifecta (
    Parser, Result
  , alphaNum, braces, chainl1, char, comma, eof, integerOrDouble
  , lower, parens, parseString, semiSep, sepBy, symbol, token, try
  )

import Numc.AST (Expr ((:+), (:-), (:*), (:/), (:=), Fun, Exe, Val, Var))

parse :: String -> Result [Expr]
parse = parseString parseMod mempty

parseMod :: Parser [Expr]
parseMod = semiSep (try parseFun <|> try parseAss <|> parseBin) <* eof

parseFun :: Parser Expr
parseFun = do
  f <- parseVar
  as <- parens $ sepBy parseVar comma
  es <- braces $ semiSep parseBin
  pure $ Fun f as es

parseExe :: Parser Expr
parseExe = do
  f <- parseVar
  as <- parens $ sepBy parseBin comma
  pure $ Exe f as

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
  fact = parens expr <|> try parseVal <|> try parseExe <|> parseVar
  addop = (:+) <$ symbol "+"
      <|> (:-) <$ symbol "-"
  mulop = (:*) <$ symbol "*"
      <|> (:/) <$ symbol "/"

parseVar :: Parser Expr
parseVar = Var <$> var
 where
  var = parens var <|> token (start <> nexts)
  start = pure <$> lower
  nexts = many $ alphaNum <|> char '_' <|> char '\''

parseVal :: Parser Expr
parseVal = Val . either fromInteger id <$> val
 where
  val = parens val <|> integerOrDouble
