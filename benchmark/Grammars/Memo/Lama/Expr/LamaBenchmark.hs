module Grammars.Memo.Lama.Expr.LamaBenchmark
  ( lamaBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (ParserState (stream), parserState)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Lama.Expr.ExprGenerator (genExpr)
import Grammars.Memo.Lama.Expr.ExprParser (basicStart)

lamaBenchmark :: Benchmark
lamaBenchmark =
  bgroup
    "LamaExpr"
    [ env
        (return $ parserState $ T.pack $ genExpr x)
        (\expr -> bench (show $ T.length $ stream expr) (nf (_parse basicStart) expr))
      | x <- [9 .. 45],
        x `mod` 9 == 0
    ]

