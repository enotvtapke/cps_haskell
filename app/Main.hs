{-# LANGUAGE OverloadedStrings #-}

module Main where

import CPS.Parser.Memo
import CPS.Stream.Stream (parserState)
import Data.Text qualified as T
import Grammars.Memo.Misc (cca)

main :: IO ()
main = do
  print $ _parse cca (parserState $ T.pack "cccccccccccccccccccccca")
