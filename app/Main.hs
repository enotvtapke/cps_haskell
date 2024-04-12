{-# LANGUAGE OverloadedStrings #-}
module Main where

import CPS.Parser.Core
import Data.Text qualified as T
import CPS.Parser.Primitives

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ _parse (single 'a' :: BaseParser k T.Text Char) "a"
  print $ _parse (regex "[0-9]*\\.[0-9]+" :: BaseParser k T.Text T.Text) "9162.420abc"
