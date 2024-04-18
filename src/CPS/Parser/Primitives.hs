{-# LANGUAGE FunctionalDependencies #-}

module CPS.Parser.Primitives
  ( single,
    oneOf,
    MonadParser (..),
  )
where

import CPS.Parser.Base (BaseParser, baseSat)
import CPS.Parser.Memo (Key (Key), Parser (..), makeStableKey)
import CPS.Stream.Regex (StreamRegex (..))
import CPS.Stream.Stream qualified as S
import Control.Applicative (Alternative (empty))
import Control.Monad (MonadPlus)
import Control.Monad.State
  ( StateT (..),
  )
import Data.Hashable (Hashable)

class (S.Stream s, MonadPlus m) => MonadParser s m | m -> s where
  satisfy :: (S.Token s -> Bool) -> m (S.Token s)
  chunk :: S.Tokens s -> m (S.Tokens s)

  -- TODO Make this function receive Regex type as an argument. In this case it will be necessary to add compileRegex function that compiles Regex with optimal parameteres

  -- | Parses according regular expression in perl-like style.
  regex :: (StreamRegex s) => String -> m (S.Tokens s)

  eof :: m ()

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

-- | This wrapper ensures that keys for regexes will not collide with other keys
newtype RegexKeyWrapper = RegexKeyWrapper String deriving (Eq, Hashable)

-- | This wrapper ensures that keys for chunks will not collide with other keys
newtype ChunkKeyWrapper s = ChunkKeyWrapper s deriving (Eq, Hashable)

-- | This wrapper is a key for all eof parsers
data EofWrapper = EofWrapper

instance (S.Stream s) => MonadParser s (Parser s) where
  satisfy :: (S.Token s -> Bool) -> Parser s (S.Token s)
  satisfy f = Parser (makeStableKey f) (satisfy f)
  eof :: Parser s ()
  eof = Parser (makeStableKey EofWrapper) eof
  chunk :: S.Tokens s -> Parser s (S.Tokens s)
  chunk s = Parser (Key $ ChunkKeyWrapper s) (chunk s)
  regex :: (StreamRegex s) => String -> Parser s (S.Tokens s)
  regex r = Parser (Key $ RegexKeyWrapper r) (regex r)

{-# INLINE single #-}
single :: (MonadParser s p) => S.Token s -> p (S.Token s)
single c = satisfy (== c)

{-# INLINE oneOf #-}
oneOf ::
  (Foldable f, MonadParser s p) =>
  -- | Collection of matching tokens
  f (S.Token s) ->
  p (S.Token s)
oneOf cs = satisfy (`elem` cs)
