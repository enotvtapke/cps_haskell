module TestHelper
  ( parseString,
  )
where

import Data.Bifunctor (bimap)
import Data.Text qualified as T
import Parser.Core (Parser, basicParse)

parseString :: Parser k T.Text T.Text -> String -> [(String, String)]
parseString p s = bimap T.unpack T.unpack <$> basicParse p (T.pack s)
