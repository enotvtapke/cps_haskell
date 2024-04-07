module Main where

import Parser.Core
import Data.Text qualified as T
import Parser.Primitives (term)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ basicParse (term "a") $ T.pack "a"
