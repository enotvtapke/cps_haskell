{-# LANGUAGE OverloadedStrings #-}

module Grammars.Base.Misc
  ( acc,
    accLongest,
    polynomial,
    indirect,
  )
where

import CPS.Parser.Base (BaseParser, baseMemo)
import CPS.Parser.Primitives (chunk, eof)
import Control.Applicative ((<|>))
import Data.Text qualified as T

indirect :: BaseParser Int T.Text T.Text
indirect = baseMemo 1 $ indirect' <|> chunk "a"
  where
    indirect' :: BaseParser Int T.Text T.Text
    indirect' = baseMemo 2 $ (<>) <$> indirect <*> chunk "b"

accLongest :: BaseParser Int T.Text T.Text
accLongest = do
  x <- acc
  x <$ eof

acc :: BaseParser Int T.Text T.Text
acc =
  baseMemo 1 $
    T.append <$> acc <*> chunk "c"
      <|> chunk "a"

polynomial :: BaseParser Int T.Text T.Text
polynomial =
  baseMemo 1 $
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
