{-# LANGUAGE TypeFamilies #-}

module CPS.Stream.Stream
  ( Stream (..),
    StreamRegex (..),
  )
where

import Data.Kind (Type)
import Data.List qualified as L
import Data.Text qualified as T
import GHC.Arr ((!))
import Text.Regex.TDFA
  ( CompOption (..),
    ExecOption (..),
    Regex,
    RegexLike (matchOnce),
    RegexMaker (makeRegexOpts),
  )

class (Eq (Token s)) => Stream s where
  -- | Type of token in the stream.
  type Token s :: Type

  uncons :: s -> Maybe (Token s, s)

  take :: Int -> s -> Maybe (s, s)

  stripPrefix :: s -> s -> Maybe s

  null :: s -> Bool

class (Stream s) => StreamRegex s where
  stripPrefixRegex :: String -> s -> Maybe (s, s)

instance Stream T.Text where
  type Token T.Text = Char
  uncons = T.uncons
  take n t = if T.compareLength t n == LT then Nothing else Just $ T.splitAt n t
  null = T.null
  stripPrefix = T.stripPrefix

instance Stream String where
  type Token String = Char
  uncons = uncons
  take n t = if length t > n then Nothing else Just $ splitAt n t
  null = Prelude.null
  stripPrefix = L.stripPrefix

instance StreamRegex T.Text where
  stripPrefixRegex r s = (`T.splitAt` s) <$> match r s

instance StreamRegex String where
  stripPrefixRegex r s = (`splitAt` s) <$> match r s

-- TODO memoize compiled regexes. While regexes are simple it is not a big deal, but in the future it may become a significt preformance issue. 
-- I suggest to put compiled Regexes in the state of parser monad
match :: (RegexLike Regex source) => String -> source -> Maybe Int
match r s = snd . (! 0) <$> matchOnce (makeRegexOpts compOption execOption ("^" <> r)) s
  where
    -- https://hackage.haskell.org/package/regex-tdfa-1.3.2.2/docs/Text-Regex-TDFA.html#t:CompOption
    execOption = ExecOption {captureGroups = False}
    compOption =
      CompOption
        { caseSensitive = True,
          multiline = False,
          rightAssoc = True,
          newSyntax = False,
          lastStarGreedy = True
        }
