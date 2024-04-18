module Grammars.Memo.Expr.ExprNoLeftRecBenchmark
  ( exprNoLeftRecBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (parserState, stream)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprGenerator (genExpr)
import Grammars.Memo.Expr.ExprNoLeftRecParser (exprStart)

exprNoLeftRecBenchmark :: Benchmark
exprNoLeftRecBenchmark =
  bgroup
    "ExprNoLeftRec"
    [env (return $ parserState $ T.pack $ show $ genExpr x) (\expr -> bench (show $ T.length $ stream expr) (nf (_parse exprStart) expr)) | x <- [1 .. 5000], x `mod` 500 == 0]
