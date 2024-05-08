module Grammars.Memo.MiscBenchmark
  ( miscBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (ParserState (..), parserState)
import Control.Monad (join)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Misc (cca, exponentional)

miscBenchmark :: Benchmark
miscBenchmark =
  bgroup
    "Misc"
    [ ccaBenchmark,
      exponentionalBenchmark
    ]

ccaBenchmark :: Benchmark
ccaBenchmark =
  bgroup
    "cca"
    [ env
        (return $ parserState $ T.pack $ replicate n 'c' <> "a")
        (\expr -> bench (show $ T.length $ stream expr) (nf (_parse cca) expr))
      | n <- [1 .. 60000],
        n `mod` 12000 == 0
    ]

exponentionalBenchmark :: Benchmark
exponentionalBenchmark =
  bgroup
    "exponentional"
    [ env
        (return $ parserState $ T.pack $ replicate (2 * n) 'a' <> join (replicate n "xy"))
        (\expr -> bench (show $ T.length $ stream expr) (nf (_parse exponentional) expr))
      | n <- [100 .. 4000],
        n `mod` 100 == 0
    ]
