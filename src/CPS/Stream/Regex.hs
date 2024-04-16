module CPS.Stream.Regex
  ( StreamRegex (..),
  )
where

import CPS.Stream.MemoFun (memoStable)
import CPS.Stream.Stream (ParserState (..))
import CPS.Stream.Stream qualified as S (Stream (..))
import Data.Data (Proxy (Proxy))
import Data.Text qualified as T
import GHC.Arr ((!))
import Text.Regex.TDFA (CompOption (..), ExecOption (ExecOption, captureGroups), Regex, RegexLike (matchOnce), RegexMaker (makeRegexOpts))

class (S.Stream s) => StreamRegex s where
  stripPrefixRegex :: String -> s -> Maybe (S.Tokens s, s)

instance StreamRegex T.Text where
  stripPrefixRegex :: String -> T.Text -> Maybe (S.Tokens T.Text, T.Text)
  stripPrefixRegex r s = (`T.splitAt` s) <$> match r s

instance StreamRegex String where
  stripPrefixRegex :: String -> String -> Maybe (S.Tokens String, String)
  stripPrefixRegex r s = (`splitAt` s) <$> match r s

instance (StreamRegex s) => StreamRegex (ParserState s) where
  stripPrefixRegex :: (StreamRegex s) => String -> ParserState s -> Maybe (S.Tokens s, ParserState s)
  stripPrefixRegex regex (ParserState stream pos) = (\(matched, stream') -> (matched, ParserState stream' (pos + S.chunkLength (Proxy :: Proxy s) matched))) <$> stripPrefixRegex regex stream

match :: (RegexLike Regex source) => String -> source -> Maybe Int
match r s = snd . (! 0) <$> matchOnce (memoStable makeRegex r) s

makeRegex :: String -> Regex
makeRegex r = makeRegexOpts compOption execOption ("^" <> r)
  where
    -- https://hackage.haskell.org/package/regex-tdfa-1.3.2.2/docs/Text-Regex-TDFA.html#t:CompOption
    execOption :: ExecOption
    execOption = ExecOption {captureGroups = False}
    compOption :: CompOption
    compOption =
      CompOption
        { caseSensitive = True,
          multiline = False,
          rightAssoc = True,
          newSyntax = False,
          lastStarGreedy = True
        }
