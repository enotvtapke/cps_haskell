module Grammars.Misc
  ( acc,
    accLongest,
    polynomial,
  )
where

import Control.Applicative ((<|>))
import Data.Text qualified as T
import Parser.Core (Parser, memo)
import Parser.Primitives (eof, term)

accLongest :: Parser Int T.Text T.Text
accLongest = do
  x <- acc
  x <$ eof

acc :: Parser Int T.Text T.Text
acc =
  memo 1 $
    ( do
        c <- acc
        T.append c <$> term "c"
    )
      <|> term "a"

polynomial :: Parser Int T.Text T.Text
polynomial =
  memo 1 $
    term "d"
      <|> do
        a1 <- term "a"
        p <- polynomial
        a2 <- term "a"
        return $ T.append (T.append a1 p) a2
      <|> do
        a1 <- term "b"
        p <- polynomial
        a2 <- term "b"
        return $ T.append (T.append a1 p) a2
      <|> do
        a1 <- term "c"
        p <- polynomial
        a2 <- term "c"
        return $ T.append (T.append a1 p) a2
