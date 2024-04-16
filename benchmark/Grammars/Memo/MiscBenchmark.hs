module Grammars.Memo.MiscBenchmark
  ( miscBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (parserState)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Misc (cca)

miscBenchmark :: Benchmark
miscBenchmark =
  bgroup
    "Misc"
    [ccaBenchmark]

ccaBenchmark :: Benchmark
ccaBenchmark =
  bgroup
    "cca"
    [env (return (n, parserState $ T.pack $ replicate n 'c' <> "a")) (\ ~(size, expr) -> bench (show size) (nf (_parse cca) expr)) | n <- [1 .. 30000], n `mod` 5000 == 0]
