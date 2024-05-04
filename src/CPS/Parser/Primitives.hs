{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}

module CPS.Parser.Primitives
  ( MonadParser (..),
  )
where

import CPS.Parser.Base (BaseParser, baseSat)
import CPS.Parser.Memo (Key (Key), Parser (..), getOrMakeKey)
import CPS.Stream.Regex (StreamRegex (..))
import CPS.Stream.Stream (Stream (tokensToChunk))
import CPS.Stream.Stream qualified as S
import Control.Applicative (Alternative (empty, many, (<|>)))
import Control.Monad (MonadPlus, replicateM)
import Control.Monad.State
  ( StateT (..),
  )
import Data.Data (Proxy (..), Typeable)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

class (S.Stream s, MonadPlus m) => MonadParser s m | m -> s where
  -- | The parser @'satisfy' f@ succeeds for any token for which the supplied
  -- function @f@ returns 'True'.
  satisfy :: (S.Token s -> Bool) -> m (S.Token s)

  -- | @'chunk' chk@ only matches the chunk @chk@.
  chunk :: S.Tokens s -> m (S.Tokens s)

  -- | @'regex' reg@ matches tokens according regular expression @reg@. @reg@ should be written in perl-like style.
  regex :: (StreamRegex s) => String -> m (S.Tokens s)

  -- | This parser only succeeds at the end of input.
  eof :: m ()

  -- | @'single' t@ only matches the single token @t@.
  single :: S.Token s -> m (S.Token s)
  {-# INLINE single #-}
  single c = satisfy (== c)

  -- | @'oneOf' ts@ succeeds if the current token is in the supplied
  -- list of tokens @ts@. Returns the parsed token.
  oneOf ::
    [S.Token s] ->
    m (S.Token s)
  {-# INLINE oneOf #-}
  oneOf cs = satisfy (`elem` cs)

  -- | Extract the specified number of tokens from the input stream and
  -- return them packed as a chunk of stream. If there is not enough tokens
  -- in the stream, empty result is returned.
  count :: m (S.Token s) -> Int -> m (S.Tokens s)
  {-# INLINE count #-}
  count p n = tokensToChunk (Proxy :: Proxy s) <$> replicateM n p

  -- | @'sepby1' p sep@ matches one or more @p@ separated by @sep@
  sepby1 :: (Typeable a, Typeable b) => m a -> m b -> m [a]
  {-# INLINE sepby1 #-}
  sepby1 p sep = (:) <$> p <*> many (sep *> p)

  -- | @'sepby' p sep@ matches zero or more @p@ separated by @sep@
  sepby :: (Typeable a, Typeable b) => m a -> m b -> m [a]
  {-# INLINE sepby #-}
  sepby p sep = sepby1 p sep <|> pure []

  {-# MINIMAL satisfy, chunk, regex, eof #-}

instance (S.Stream s) => MonadParser s (BaseParser k s) where
  satisfy :: (S.Token s -> Bool) -> BaseParser k s (S.Token s)
  satisfy f =
    StateT
      ( \s ->
          case S.uncons s of
            Just (c, s') -> if f c then return (c, s') else empty
            Nothing -> empty
      )
  chunk :: S.Tokens s -> BaseParser k s (S.Tokens s)
  chunk s =
    StateT
      ( \state ->
          case S.stripPrefix s state of
            Just s' -> return (s, s')
            Nothing -> empty
      )
  regex :: (StreamRegex s) => String -> BaseParser k s (S.Tokens s)
  regex r =
    StateT
      ( \s ->
          case stripPrefixRegex r s of
            Just (t, x) -> return (t, x)
            Nothing -> empty
      )
  eof :: BaseParser k s ()
  eof = baseSat S.null

data PrimitiveKey = EofKey | ChunkKey | RegexKey | SingleKey | OneOfKey | CountKey | SepByKey | SepBy1Key deriving (Generic, Eq, Hashable)

instance (S.Stream s) => MonadParser s (Parser s) where
  satisfy :: (S.Token s -> Bool) -> Parser s (S.Token s)
  satisfy f = Parser Nothing $ satisfy f
  eof :: Parser s ()
  eof = Parser (Just $ Key EofKey) eof
  chunk :: S.Tokens s -> Parser s (S.Tokens s)
  chunk s = Parser (Just $ Key (ChunkKey, s)) $ chunk s
  regex :: (StreamRegex s) => String -> Parser s (S.Tokens s)
  regex r = Parser (Just $ Key (RegexKey, r)) $ regex r
  single :: S.Token s -> Parser s (S.Token s)
  single c = Parser (Just $ Key (SingleKey, c)) $ single c
  oneOf :: [S.Token s] -> Parser s (S.Token s)
  oneOf cs = Parser (Just $ Key (OneOfKey, cs)) $ oneOf cs
  count :: Parser s (S.Token s) -> Int -> Parser s (S.Tokens s)
  count p n = Parser (Just $ Key (CountKey, getOrMakeKey p, n)) $ count (parser p) n
  sepby1 :: (Typeable a, Typeable b) => Parser s a -> Parser s b -> Parser s [a]
  sepby1 p sep = Parser (Just $ Key (SepBy1Key, getOrMakeKey p, getOrMakeKey sep)) $ sepby1 (parser p) (parser sep)
  sepby :: (Typeable a, Typeable b) => Parser s a -> Parser s b -> Parser s [a]
  sepby p sep = Parser (Just $ Key (SepByKey, getOrMakeKey p, getOrMakeKey sep)) $ sepby (parser p) (parser sep)
