{-# LANGUAGE OverloadedStrings #-}
module Main where

import CPS.Parser.Memo
import Data.Text qualified as T
import CPS.Parser.Primitives

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ _parse (single 'a' :: Parser T.Text Char) "a"
  print $ _parse (regex "[0-9]*\\.[0-9]+" :: Parser T.Text T.Text) "9162.420abc"
