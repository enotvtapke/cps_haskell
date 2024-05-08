module Main (main) where

import Criterion.IO (readJSONReports)
import Criterion.Main
import Criterion.Types
  ( Config (..),
    Regression (regCoeffs, regResponder),
    Report (reportAnalysis, reportName),
    SampleAnalysis (anRegress),
    jsonFile,
  )
import Data.ByteString.Lazy qualified as B
import Data.Csv qualified as Csv
import Data.Foldable (find)
import Data.Map ((!))
import Data.Maybe (fromMaybe)
import Grammars.Base.Expr.ExprFastBenchmark (exprFastBaseBenchmark)
import Grammars.Megaparsec.ExprNoLeftRecBenchmark (exprNoLeftRecMegaparsecBenchmark)
import Grammars.Memo.Expr.ExprFastBenchmark (exprFastMemoBenchmark)
import Grammars.Memo.Expr.ExprNoLeftRecBenchmark (exprNoLeftRecBenchmark)
import Grammars.Memo.Lama.Expr.LamaBenchmark (lamaBenchmark)
import Grammars.Memo.MiscBenchmark (miscBenchmark)
import Statistics.Types

main :: IO ()
main =
  defaultMainWith
    defaultConfig
      { regressions = [(["iters"], "allocated"), (["iters"], "peakMbAllocated")],
        csvFile = Just csvFilePath,
        jsonFile = Just jsonFilePath,
        timeLimit = 5
      }
    [ bgroup
        "benchmark"
        [ 
          exprFastBaseBenchmark,
          exprFastMemoBenchmark,
          exprNoLeftRecBenchmark,
          exprNoLeftRecMegaparsecBenchmark,
          lamaBenchmark,
          miscBenchmark
        ]
    ]
    >> reportMemoryUsage jsonFilePath

csvFilePath :: FilePath
csvFilePath = "./benchmark/reports/report.csv"

jsonFilePath :: FilePath
jsonFilePath = "./benchmark/reports/report.json"

reportMemoryUsage :: FilePath -> IO ()
reportMemoryUsage jsonFile =
  do
    rpts <- do
      res <- readJSONReports jsonFile
      case res of
        Left err -> error $ "error reading file " ++ jsonFile ++ ":\n  " ++ show err
        Right (_, _, rs) -> return rs
    let names = map reportName rpts
    let peakMbAllocated = responderCoef rpts "peakMbAllocated" "y"
    let allocated = responderCoef rpts "allocated" "iters"
    writeCsv csvFilePath [("Name", "PeakMbAllocated", "Allocated")]
    writeCsv csvFilePath $ zip3 names peakMbAllocated allocated
    return ()
  where
    responderCoef :: [Report] -> String -> String -> [Double]
    responderCoef rpts responder coef =
      map
        ( \report ->
            (\reg -> estPoint $ regCoeffs reg ! coef) $
              fromMaybe undefined $
                find (\reg -> regResponder reg == responder) (anRegress $ reportAnalysis report)
        )
        rpts
    writeCsv :: (Csv.ToRecord a) => FilePath -> [a] -> IO ()
    writeCsv csvFile val = do
      B.appendFile csvFile (Csv.encode val)
      return ()
