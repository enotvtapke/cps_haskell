{-# LANGUAGE OverloadedStrings #-}
module Main where

import CPS.Parser.Core ( _parse, Parser )
import Data.Text qualified as T
import CPS.Parser.Primitives (char)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ _parse (char 'a' :: Parser k T.Text Char) "a"
