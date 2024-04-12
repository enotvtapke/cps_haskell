{-# LANGUAGE OverloadedStrings #-}

module Grammars.Memo.Misc
  ( acc,
    accLongest,
    polynomial,
    indirect,
  )
where

import CPS.Parser.Memo (Parser, memo)
import CPS.Parser.Primitives (chunk, eof)
import Control.Applicative ((<|>))
import Data.Text qualified as T

indirect :: Parser T.Text T.Text
indirect = memo $ indirect' <|> chunk "a"
  where
    indirect' :: Parser T.Text T.Text
    indirect' = memo $ (<>) <$> indirect <*> chunk "b"

accLongest :: Parser T.Text T.Text
accLongest = do
  x <- acc
  x <$ eof

acc :: Parser T.Text T.Text
acc =
  memo $
    T.append <$> acc <*> chunk "c"
      <|> chunk "a"

polynomial :: Parser T.Text T.Text
polynomial =
  memo $
    chunk "d"
      <|> do
        l <- chunk "a"
        p <- polynomial
        r <- chunk "a"
        return $ l <> p <> r
      <|> do
        l <- chunk "b"
        p <- polynomial
        r <- chunk "b"
        return $ l <> p <> r
      <|> do
        l <- chunk "c"
        p <- polynomial
        r <- chunk "c"
        return $ l <> p <> r
