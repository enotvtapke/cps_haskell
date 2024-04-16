{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module CPS.Stream.Stream
  ( Stream (..),
    ParserState (..),
    parserState,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor (second))
import Data.Data (Typeable)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Kind (Type)
import Data.List qualified as L
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prelude hiding (length, null)

class (Typeable s, Eq (Token s), Typeable (Token s), Eq (Tokens s), Hashable (Tokens s), Typeable (Tokens s)) => Stream s where
  type Token s :: Type

  type Tokens s :: Type

  uncons :: s -> Maybe (Token s, s)

  stripPrefix :: Tokens s -> s -> Maybe s

  null :: s -> Bool

  chunkLength :: Proxy s -> Tokens s -> Int

instance Stream T.Text where
  type Token T.Text = Char
  type Tokens T.Text = T.Text
  uncons :: T.Text -> Maybe (Token T.Text, T.Text)
  uncons = T.uncons
  null :: T.Text -> Bool
  null = T.null
  stripPrefix :: Tokens T.Text -> T.Text -> Maybe T.Text
  stripPrefix = T.stripPrefix
  chunkLength :: Proxy s -> Tokens T.Text -> Int
  chunkLength _ = T.length

instance Stream String where
  type Token String = Char
  type Tokens String = String
  uncons :: String -> Maybe (Token String, String)
  uncons = L.uncons
  null :: String -> Bool
  null = L.null
  stripPrefix :: Tokens String -> String -> Maybe String
  stripPrefix = L.stripPrefix
  chunkLength :: Proxy s -> Tokens String -> Int
  chunkLength _ = L.length

data ParserState s = ParserState {stream :: s, pos :: Int} deriving (Generic, Show, NFData)

parserState :: s -> ParserState s
parserState s = ParserState s 0

instance (Stream s) => Stream (ParserState s) where
  type Token (ParserState s) = Token s
  type Tokens (ParserState s) = Tokens s
  uncons :: ParserState s -> Maybe (Token (ParserState s), ParserState s)
  uncons (ParserState stream pos) = second (\stream' -> ParserState stream' (pos + 1)) <$> uncons stream
  stripPrefix :: Tokens s -> ParserState s -> Maybe (ParserState s)
  stripPrefix prefix (ParserState stream pos) = (\stream' -> ParserState stream' (pos + chunkLength (Proxy :: Proxy s) prefix)) <$> stripPrefix prefix stream
  null :: ParserState s -> Bool
  null (ParserState stream _) = null stream
  chunkLength :: Proxy (ParserState s) -> Tokens (ParserState s) -> Int
  chunkLength = chunkLength

instance Eq (ParserState s) where
  (==) :: ParserState s -> ParserState s -> Bool
  (==) (ParserState _ pos1) (ParserState _ pos2) = pos1 == pos2

instance Hashable (ParserState s) where
  hashWithSalt :: Int -> ParserState s -> Int
  hashWithSalt s (ParserState _ pos) = hashWithSalt s pos
