module Parser.Primitives
  ( term,
    eof,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad.State
  ( StateT (..),
  )
import Data.Text qualified as T
import Parser.Core (Parser, sat)

termInternal :: T.Text -> Parser k T.Text T.Text
termInternal t =
  StateT
    ( \s ->
        case T.stripPrefix t s of
          Just x -> return (t, x)
          Nothing -> empty
    )

term :: String -> Parser Int T.Text T.Text
term = termInternal . T.pack

eof :: Parser k T.Text ()
eof = sat T.null
