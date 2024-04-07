module CPS.Parser.Primitives
  ( sat,
    char,
    str,
    regex,
    eof,
  )
where

import CPS.Parser.Core (Parser, _sat)
import CPS.Stream.Stream qualified as S
import Control.Applicative (Alternative (empty))
import Control.Monad.State
  ( StateT (..),
  )

sat :: (S.Stream s) => (S.Token s -> Bool) -> Parser k s (S.Token s)
sat f =
  StateT
    ( \s ->
        case S.uncons s of
          Just (c, s') -> if f c then return (c, s') else empty
          Nothing -> empty
    )

char :: (S.Stream s) => S.Token s -> Parser k s (S.Token s)
char c = sat (== c)

str :: (S.Stream s) => s -> Parser k s s
str s =
  StateT
    ( \state ->
        case S.stripPrefix s state of
          Just s' -> return (s, s')
          Nothing -> empty
    )

-- | Parses according regular expression in perl-like style.
regex :: (S.StreamRegex s) => String -> Parser k s s
regex r =
  StateT
    ( \s ->
        case S.stripPrefixRegex r s of
          Just (t, x) -> return (t, x)
          Nothing -> empty
    )

eof :: (S.Stream s) => Parser k s ()
eof = _sat S.null
