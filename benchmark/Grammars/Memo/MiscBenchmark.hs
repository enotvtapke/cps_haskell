module Grammars.Memo.MiscBenchmark
  ( miscBenchmark,
  )
where

import CPS.Parser.Memo (_parse)
import CPS.Stream.Stream (ParserState (..), parserState)
import Control.Monad (join)
import Criterion.Main
import Data.Text qualified as T
import Grammars.Memo.Misc (cca, exponential, exponentialUnmemoized)

miscBenchmark :: Benchmark
miscBenchmark =
  bgroup
    "Misc"
    [ exponentialUnmemoizedBenchmark,
      ccaBenchmark,
      exponentialBenchmark
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

exponentialBenchmark :: Benchmark
exponentialBenchmark =
  bgroup
    "exponential"
    [ env
        (return $ parserState $ T.pack $ replicate (2 * n) 'a' <> join (replicate n "xy"))
        (\expr -> bench (show $ T.length $ stream expr) (nf (_parse exponential) expr))
      | n <- [100 .. 4000],
        n `mod` 100 == 0
    ]

exponentialUnmemoizedBenchmark :: Benchmark
exponentialUnmemoizedBenchmark =
  bgroup
    "exponentialUnmemoized"
    [ env
        (return $ parserState $ T.pack $ replicate n 'a' <> join (replicate (n `div` 2) "xy") <> if n `mod` 2 == 1 then "x" else "")
        (\expr -> bench (show $ T.length $ stream expr) (nf (_parse exponentialUnmemoized) expr))
      | n <- [1 .. 20]
    ]
