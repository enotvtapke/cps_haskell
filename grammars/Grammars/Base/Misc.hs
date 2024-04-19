{-# LANGUAGE OverloadedStrings #-}

module Grammars.Base.Misc
  ( acc,
    accLongest,
    palindrom,
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

palindrom :: BaseParser Int T.Text T.Text
palindrom =
  baseMemo 1 $
    chunk "d"
      <|> do
        l <- chunk "a"
        p <- palindrom
        r <- chunk "a"
        return $ l <> p <> r
      <|> do
        l <- chunk "b"
        p <- palindrom
        r <- chunk "b"
        return $ l <> p <> r
      <|> do
        l <- chunk "c"
        p <- palindrom
        r <- chunk "c"
        return $ l <> p <> r
