module Grammars.Memo.Expr.ExprFastBenchmark
  ( exprFastBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (parserState, stream)
import Control.Monad (join)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprFastParser (exprStart)
import Grammars.Memo.Expr.ExprGenerator (genExpr)

exprFastBenchmark :: Benchmark
exprFastBenchmark =
  bgroup
    "ExprFast"
    [linearBenchmark, randomBenchmark]

linearBenchmark :: Benchmark
linearBenchmark =
  bgroup
    "linear"
    [env (return $ parserState $ T.pack $ "1" <> join (replicate x "+1")) (\expr -> bench (show $ T.length $ stream expr) (nf (_parse exprStart) expr)) | x <- [750 .. 7500], x `mod` 750 == 0]

randomBenchmark :: Benchmark
randomBenchmark =
  bgroup
    "random"
    [env (return $ parserState $ T.pack $ show $ genExpr x) (\expr -> bench (show $ T.length $ stream expr) (nf (_parse exprStart) expr)) | x <- [1 .. 5000], x `mod` 500 == 0]
