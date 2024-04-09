module Grammars.Expr.ExprGenerator (genExpr) where

import GHC.IO (unsafePerformIO)
import Grammars.Expr.Expr
import System.Random (randomRIO)

f :: Int -> IO F
f n = if n <= 0 then FVal <$> randomRIO (1, 99) else FExpr <$> expr (n - 1)

term :: Int -> IO Term
term n =
  do
    r <- randomRIO (1 :: Int, 2)
    let rr = if n <= 0 then 1 else r
    case rr of
      1 -> TermVal <$> f n
      2 -> TermOp <$> term (n `div` 2) <*> ((["*", "/"] !!) <$> randomRIO (0, 1)) <*> f (n `div` 2)
      _ -> undefined

expr :: Int -> IO Expr
expr n =
  do
    r <- randomRIO (1 :: Int, 2)
    let rr = if n <= 0 then 1 else r
    case rr of
      1 -> ExprVal <$> term n
      2 -> ExprOp <$> expr (n `div` 2) <*> ((["+", "-"] !!) <$> randomRIO (0, 1)) <*> term (n `div` 2)
      _ -> undefined

genExpr :: Int -> Expr
genExpr n = unsafePerformIO $ expr n
