{-# OPTIONS_GHC -Wno-orphans #-}

module AST where

import Control.Applicative (liftA2)
import Test.QuickCheck (Arbitrary (arbitrary), oneof, sized, quickCheck)
import Text.Trifecta (eof, foldResult, parseString)

import Numc.AST (Expr ((:+), (:-), (:*), (:/), Val))
import Numc.Parser (parseBin)

instance Arbitrary Expr where
  arbitrary = sized expr
   where
    expr 0 = Val <$> arbitrary
    expr n = oneof
              [
                expr 0
              , liftA2 (:+) (subexpr n) (subexpr n)
              , liftA2 (:-) (subexpr n) (subexpr n)
              , liftA2 (:*) (subexpr n) (subexpr n)
              , liftA2 (:/) (subexpr n) (subexpr n)
              ]
    subexpr n = expr (n `div` 2)

testReadShow :: IO ()
testReadShow = quickCheck $ do
  let parse = foldResult (const Nothing) Just . parseString (parseBin <* eof) mempty
  \e -> parse (show e) == Just e