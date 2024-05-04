module Grammars.Base.Expr.ExprFastBenchmark
  ( exprFastBaseBenchmark,
  )
where

import CPS.Parser.Base (baseParse)
import CPS.Stream.Stream (parserState, stream)
import Control.Monad (join)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Base.Expr.ExprFastParser (exprStart)
import Grammars.Memo.Expr.ExprGenerator (genExpr)

exprFastBaseBenchmark :: Benchmark
exprFastBaseBenchmark =
  bgroup
    "ExprFastBase"
    [linearBenchmark, randomBenchmark]

linearBenchmark :: Benchmark
linearBenchmark =
  bgroup
    "linear"
    [ env
        (return $ parserState $ T.pack $ "1" <> join (replicate x "+1"))
        (\expr -> bench (show $ T.length $ stream expr) (nf (baseParse exprStart) expr))
      | x <- [750 .. 7500],
        x `mod` 750 == 0
    ]

randomBenchmark :: Benchmark
randomBenchmark =
  bgroup
    "random"
    [ env
        (return $ parserState $ T.pack $ show $ genExpr x)
        (\expr -> bench (show $ T.length $ stream expr) (nf (baseParse exprStart) expr))
      | x <- [1 .. 20000],
        x `mod` 5000 == 0
    ]
