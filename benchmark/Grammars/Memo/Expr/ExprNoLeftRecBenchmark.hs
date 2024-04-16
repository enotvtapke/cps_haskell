module Grammars.Memo.Expr.ExprNoLeftRecBenchmark
  ( exprNoLeftRecBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (parserState)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprGenerator (genExpr)
import Grammars.Memo.Expr.ExprNoLeftRecParser (exprStart)

exprNoLeftRecBenchmark :: Benchmark
exprNoLeftRecBenchmark =
  bgroup
    "ExprNoLeftRec"
    [env (return (x, parserState $ T.pack $ show $ genExpr x)) (\ ~(size, expr) -> bench (show size) (nf (_parse exprStart) expr)) | x <- [1 .. 5000], x `mod` 500 == 0]
