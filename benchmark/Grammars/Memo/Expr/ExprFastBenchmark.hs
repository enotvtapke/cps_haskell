module Grammars.Memo.Expr.ExprFastBenchmark
  ( exprFastBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (parserState, stream)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprFastParser (exprStart)
import Grammars.Memo.Expr.ExprGenerator (genExpr)

exprFastBenchmark :: Benchmark
exprFastBenchmark =
  bgroup
    "ExprFast"
    [env (return $ parserState $ T.pack $ show $ genExpr x) (\expr -> bench (show $ T.length $ stream expr) (nf (_parse exprStart) expr)) | x <- [1 .. 5000], x `mod` 500 == 0]
