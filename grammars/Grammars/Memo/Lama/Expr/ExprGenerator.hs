module Grammars.Memo.Lama.Expr.ExprGenerator
  ( genExpr,
  )
where

import Control.Monad (join)
import System.IO.Unsafe (unsafePerformIO)

genExpr :: Int -> String
genExpr n = join (replicate (n - 1) $ fileRead "left.txt") <> fileRead "middle.txt" <> join (replicate (n - 1) $ fileRead "right.txt")

fileRead :: String -> String
fileRead fileName = unsafePerformIO $ readFile ("grammars/Grammars/Memo/Lama/Expr/input/" <> fileName)
