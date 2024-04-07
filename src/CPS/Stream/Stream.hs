{-# LANGUAGE TypeFamilies #-}

module CPS.Stream.Stream
  ( Stream (..),
  )
where

import Data.Kind (Type)
import Data.Text qualified as T

class (Eq (Token s)) => Stream s where
  -- | Type of token in the stream.
  type Token s :: Type

  uncons :: s -> Maybe (Token s, s)

  take :: Int -> s -> Maybe (s, s)

  stripPrefix :: s -> s -> Maybe s

  null :: s -> Bool

instance Stream T.Text where
  type Token T.Text = Char
  uncons = T.uncons
  take n t = if T.compareLength t n == LT then Nothing else Just $ T.splitAt n t
  null = T.null
  stripPrefix = T.stripPrefix
