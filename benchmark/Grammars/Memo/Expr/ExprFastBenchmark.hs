module Grammars.Memo.Expr.ExprFastBenchmark
  ( exprFastBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (parserState)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprFastParser (exprStart)
import Grammars.Memo.Expr.ExprGenerator (genExpr)

exprFastBenchmark :: Benchmark
exprFastBenchmark =
  bgroup
    "ExprFast"
    [env (return (x, parserState $ T.pack $ show $ genExpr x)) (\ ~(size, expr) -> bench (show size) (nf (_parse exprStart) expr)) | x <- [1 .. 5000], x `mod` 500 == 0]
