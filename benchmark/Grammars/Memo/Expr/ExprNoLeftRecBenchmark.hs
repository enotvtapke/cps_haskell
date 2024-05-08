module Grammars.Memo.Expr.ExprNoLeftRecBenchmark
  ( exprNoLeftRecBenchmark,
  )
where

import CPS.Parser.Base (baseParse)
import CPS.Stream.Stream (parserState, stream)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Expr.ExprGenerator (genExpr)
import Grammars.Memo.Expr.ExprNoLeftRecParser (exprStart)

exprNoLeftRecBenchmark :: Benchmark
exprNoLeftRecBenchmark =
  bgroup
    "ExprNoLeftRec"
    [ env
        (return $ parserState $ T.pack $ show $ genExpr x)
        (\expr -> bench (show $ T.length $ stream expr) (nf (baseParse exprStart) expr))
      | x <- [1 .. 66000],
        x `mod` 660 == 0
    ]
