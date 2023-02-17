module Numc.Parser where

import Control.Applicative ((<|>), Alternative (many))
import Text.Trifecta (
    Parser, Result, TokenParsing
  , alphaNum, between, chainl1, char, eof, integerOrDouble, lower, parens, parseString, symbol, try, whiteSpace
  )

import Numc.AST (Expr ((:+), (:-), (:*), (:/), Val, Var))

parseExpr :: String -> Result Expr
parseExpr = parseString (parseBin <* eof) mempty

parseBin :: Parser Expr
parseBin = expr
 where
  expr = chainl1 term addop
  term = chainl1 val mulop
  val = parens expr <|> ignoreSpace (try parseVal <|> parseVar)
  addop = (:+) <$ symbol "+"
      <|> (:-) <$ symbol "-"
  mulop = (:*) <$ symbol "*"
      <|> (:/) <$ symbol "/"

parseVar :: Parser Expr
parseVar = Var <$> var
 where
  var = ignoreSpace $ parens var <|> start <> mid <> end
  start = pure <$> lower
  mid = many $ alphaNum <|> char '_'
  end = fmap pure (alphaNum <|> char '\'') <|> mempty

parseVal :: Parser Expr
parseVal = Val . either fromInteger id <$> val
 where
  val = parens val <|> integerOrDouble

ignoreSpace :: (TokenParsing m) => m a -> m a
ignoreSpace = between whiteSpace whiteSpace
