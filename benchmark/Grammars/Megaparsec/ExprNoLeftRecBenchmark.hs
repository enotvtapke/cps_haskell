module Grammars.Megaparsec.ExprNoLeftRecBenchmark
  ( exprNoLeftRecMegaparsecBenchmark,
  )
where

import Criterion.Main
import Data.Text qualified as T
import Debug.Trace (trace)
import Grammars.Megaparsec.ExprNoLeftRecParser (exprStart)
import Grammars.Memo.Expr.ExprGenerator (genExpr)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Error (ShowErrorComponent)
import Text.Megaparsec.Stream (TraversableStream, VisualStream)

exprNoLeftRecMegaparsecBenchmark :: Benchmark
exprNoLeftRecMegaparsecBenchmark =
  bgroup
    "ExprNoLeftRecMegaparsec"
    [ env
        (return $ T.pack $ show $ genExpr x)
        (\expr -> bench (show $ T.length expr) (nf (_parse exprStart) expr))
      | x <- [1 .. 66000],
        x `mod` 660 == 0
    ]

_parse :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s a -> s -> a
_parse p input = case parse p "" input of
  Left bundle -> trace (errorBundlePretty bundle) undefined
  Right xs -> xs
