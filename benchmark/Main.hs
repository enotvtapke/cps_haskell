module Main where

import Criterion.Main
import Criterion.Types (Config (..))
import Grammars.Memo.Expr.ExprFastBenchmark (exprFastBenchmark)
import Grammars.Memo.Expr.ExprNoLeftRecBenchmark (exprNoLeftRecBenchmark)
import Grammars.Memo.Lama.Expr.LamaBenchmark (lamaBenchmark)
import Grammars.Memo.MiscBenchmark (miscBenchmark)

main :: IO ()
main =
  defaultMainWith
    defaultConfig {timeLimit = 1 {- resamples = 2, -}, csvFile = Just "./benchmark/reports/report.csv"}
    [ bgroup
        "benchmark"
        [ 
          exprFastBenchmark,
          exprNoLeftRecBenchmark,
          miscBenchmark,
          lamaBenchmark
        ]
    ]
