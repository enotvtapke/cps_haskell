{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module CPS.Parser.Memo
  ( Key (..),
    makeStableKey,
    makeUniqueKey,
    Parser (..),
    memo,
    memoWithKey,
    _parse,
  )
where

import CPS.Parser.Base (BaseParser, baseMemo, baseParse)
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Data.Data (Typeable)
import Data.Hashable (Hashable (hashWithSalt), hash)
import Data.Typeable (cast)
import Data.Unique (newUnique)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (makeStableName)

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

-- | In order to use this function use '{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}' pragma. Otherwise repeated calls to this function will be merged into one and generated keys will not be unique.
-- This function is disgustingly fragile and unsafe. Do not use it from outside. It is temporarily exported for test purposes.
{-# NOINLINE makeUniqueKey #-}
makeUniqueKey :: a -> Key
makeUniqueKey _ = Key $ unsafePerformIO newUnique

data Parser s a = Parser {key :: Key, parser :: BaseParser Key s a}

{-# INLINE parserWithRandomKey #-}
parserWithRandomKey :: BaseParser Key s t -> Parser s t
parserWithRandomKey = Parser $ makeUniqueKey ()

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f p = parserWithRandomKey (f <$> parser p)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = parserWithRandomKey (pure x)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) f p = parserWithRandomKey (parser f <*> parser p)

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) p f = parserWithRandomKey (parser p >>= (parser . f))

instance Alternative (Parser s) where
  -- TODO make it: Parser (makeStableKey (empty :: BaseParser Key s a)) empty to make all empty parsers keys equal
  empty :: Parser s a
  empty = parserWithRandomKey empty
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) p1 p2 = parserWithRandomKey (parser p1 <|> parser p2)

instance MonadPlus (Parser s)

_parse :: (Typeable s, Typeable a, Hashable s) => Parser s a -> s -> [(a, s)]
_parse p = baseParse (parser p)

memo :: (Typeable s, Typeable a, Hashable s, Eq s) => Parser s a -> Parser s a
memo p = Parser k (baseMemo k (parser p))
  where
    k = makeStableKey p

memoWithKey :: (Typeable s, Typeable a, Hashable s, Eq s) => Key -> Parser s a -> Parser s a
memoWithKey k p = Parser k (baseMemo k (parser p))
