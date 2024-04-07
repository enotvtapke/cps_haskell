{-# LANGUAGE OverloadedStrings #-}

module Grammars.Misc
  ( acc,
    accLongest,
    polynomial,
  )
where

import CPS.Parser.Core (Parser, memo)
import CPS.Parser.Primitives (eof, str)
import Control.Applicative ((<|>))
import Data.Text qualified as T

accLongest :: Parser Int T.Text T.Text
accLongest = do
  x <- acc
  x <$ eof

acc :: Parser Int T.Text T.Text
acc =
  memo 1 $
    T.append <$> acc <*> str "c"
      <|> str "a"

polynomial :: Parser Int T.Text T.Text
polynomial =
  memo 1 $
    str "d"
      <|> do
        l <- str "a"
        p <- polynomial
        r <- str "a"
        return $ l <> p <> r
      <|> do
        l <- str "b"
        p <- polynomial
        r <- str "b"
        return $ l <> p <> r
      <|> do
        l <- str "c"
        p <- polynomial
        r <- str "c"
        return $ l <> p <> r
