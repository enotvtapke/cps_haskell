{-# LANGUAGE DeriveAnyClass #-}

module CPS.Parser.Memo
  ( Key (..),
    makeStableKey,
    Parser (..),
    memo,
    memoWithKey,
    _parse,
    getOrMakeKey,
  )
where

import CPS.Parser.Base (BaseParser, baseMemo, baseParse, DeterministicAlternative (..))
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Data.Data (Typeable)
import Data.Hashable (Hashable (hashWithSalt), hash)
import Data.Typeable (cast)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (makeStableName)
import GHC.Generics (Generic)

data Key = forall a. (Typeable a, Hashable a, Eq a) => Key a

instance Show Key where
  show :: Key -> String
  show (Key a) = "Key@" <> show (hash a)

instance Eq Key where
  (==) :: Key -> Key -> Bool
  (==) (Key a) (Key b) =
    case cast b of
      Just x -> a == x
      Nothing -> False

instance Hashable Key where
  hashWithSalt :: Int -> Key -> Int
  hashWithSalt s (Key a) = hashWithSalt s a

-- | For arguments with the same addresses this function returns the same Key. The opposite is incorrect:
-- for arguments with maybe (depending on optimization level) different addresses the same Key may be returned.
makeStableKey :: (Typeable a) => a -> Key
makeStableKey a = Key (unsafePerformIO $ makeStableName a)

data Parser s a = Parser {key :: Maybe Key, parser :: BaseParser Key s a}

data Label = EmptyLabel deriving (Generic, Eq, Hashable)

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f p = Parser Nothing (f <$> parser p)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser Nothing (pure x)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) f p = Parser Nothing (parser f <*> parser p)

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) p f = Parser Nothing (parser p >>= (parser . f))

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (Just $ Key EmptyLabel) empty
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) p1 p2 = Parser Nothing (parser p1 <|> parser p2)

instance MonadPlus (Parser s)

instance DeterministicAlternative (Parser s) where
  (</>) :: Parser s a -> Parser s a -> Parser s a
  (</>) p1 p2 = Parser Nothing (parser p1 </> parser p2) 

_parse :: (Typeable s, Typeable a, Hashable s) => Parser s a -> s -> [(a, s)]
_parse p = baseParse (parser p)

memo :: (Typeable s, Typeable a, Hashable s, Eq s) => Parser s a -> Parser s a
memo p = Parser (Just k) (baseMemo k (parser p))
  where
    k = makeStableKey p

getOrMakeKey :: (Typeable s, Typeable a) => Parser s a -> Key
getOrMakeKey (Parser (Just k) _) = k
getOrMakeKey p@(Parser Nothing _) = makeStableKey p

memoWithKey :: (Typeable s, Typeable a, Hashable s, Eq s) => Key -> Parser s a -> Parser s a
memoWithKey k p = Parser (Just k) (baseMemo k (parser p))
