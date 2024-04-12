module CPS.Parser.Memo
  ( Key (..),
    makeStableKey,
    makeRandomStableKey,
    Parser (..),
    memo,
    memoWithKey,
    _parse
  )
where

import CPS.Parser.Base (BaseParser, baseMemo, baseParse)
import Control.Applicative (Alternative)
import Data.Data (Typeable)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Typeable (cast)
import GHC.Base (Alternative (..), MonadPlus)
import GHC.IO (unsafePerformIO)
import GHC.StableName (makeStableName)

data Key = forall a. (Typeable a, Hashable a, Eq a) => Key a

instance Eq Key where
  (==) :: Key -> Key -> Bool
  (==) (Key a) (Key b) =
    case cast b of
      Just x -> a == x
      _ -> False

instance Hashable Key where
  hashWithSalt :: Int -> Key -> Int
  hashWithSalt s (Key a) = hashWithSalt s a

makeStableKey :: (Typeable a) => a -> Key
makeStableKey a = Key (unsafePerformIO $ makeStableName a)

makeRandomStableKey :: Key
makeRandomStableKey = Key (unsafePerformIO $ makeStableName ())

data Parser s t = Parser {key :: Key, parser :: BaseParser Key s t}

parserWithRandomKey :: BaseParser Key s t -> Parser s t
parserWithRandomKey = Parser makeRandomStableKey

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f p = parserWithRandomKey (f <$> parser p)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure p = parserWithRandomKey (pure p)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) f p = parserWithRandomKey (parser f <*> parser p)

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) p f = parserWithRandomKey (parser p >>= (parser . f))

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = parserWithRandomKey empty
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) p1 p2 = parserWithRandomKey (parser p1 <|> parser p2)

instance MonadPlus (Parser s)

_parse :: (Typeable s, Typeable t, Hashable s) => Parser s t -> s -> [(t, s)]
_parse p = baseParse (parser p)

memo :: (Typeable s, Typeable t, Hashable s, Eq s) => Parser s t -> Parser s t
memo p = Parser k (baseMemo k (parser p))
  where
    k = makeStableKey p

memoWithKey :: (Typeable s, Typeable t, Hashable s, Eq s) => Key -> Parser s t -> Parser s t
memoWithKey k p = Parser k (baseMemo k (parser p))
