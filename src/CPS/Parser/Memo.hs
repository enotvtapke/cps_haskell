{-# LANGUAGE OverloadedStrings #-}

module CPS.Parser.Memo where

import CPS.Parser.Core (Parser, memo, _parse)
import CPS.Stream.Stream qualified as S
import Control.Applicative (Alternative)
import Control.Monad.State (StateT (..))
import Data.Data (Typeable)
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.Text qualified as T
import Data.Typeable (cast)
import Debug.Trace (trace)
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

data ParserWithKey s t = ParserWithKey {parser :: Parser Key s t, key :: Key}

parserWithKey :: Parser Key s t -> ParserWithKey s t
parserWithKey p = ParserWithKey p makeRandomStableKey

instance Functor (ParserWithKey s) where
  fmap :: (a -> b) -> ParserWithKey s a -> ParserWithKey s b
  fmap f p = parserWithKey (f <$> parser p)

instance Applicative (ParserWithKey s) where
  pure :: a -> ParserWithKey s a
  pure p = parserWithKey (pure p)
  (<*>) :: ParserWithKey s (a -> b) -> ParserWithKey s a -> ParserWithKey s b
  (<*>) f p = parserWithKey (parser f <*> parser p)

instance Monad (ParserWithKey s) where
  (>>=) :: ParserWithKey s a -> (a -> ParserWithKey s b) -> ParserWithKey s b
  (>>=) p f = parserWithKey (parser p >>= (parser . f))

instance Alternative (ParserWithKey s) where
  empty :: ParserWithKey s a
  empty = parserWithKey empty
  (<|>) :: ParserWithKey s a -> ParserWithKey s a -> ParserWithKey s a
  (<|>) p1 p2 = parserWithKey (parser p1 <|> parser p2)

instance MonadPlus (ParserWithKey s)

string :: (Typeable s, Hashable s, S.Stream s) => s -> ParserWithKey s s
string s =
  ParserWithKey
    ( StateT
        ( \state ->
            case S.stripPrefix s state of
              Just s' -> return (s, s')
              Nothing -> empty
        )
    )
    (Key s)

parse :: (Typeable s, Typeable t, Hashable s) => ParserWithKey s t -> s -> [(t, s)]
parse p = _parse (parser p)

mapKey :: (Key -> Key) -> ParserWithKey s t -> ParserWithKey s t
mapKey f p = ParserWithKey (parser p) (f $ key p)

smartMemo :: (Typeable s, Typeable t, Hashable s, Eq s) => ParserWithKey s t -> ParserWithKey s t
smartMemo p = ParserWithKey (memo k (parser p)) k
  where
    k = makeStableKey p

smartMemoWithKey :: (Typeable s, Typeable t, Hashable s, Eq s) => Key -> ParserWithKey s t -> ParserWithKey s t
smartMemoWithKey k p = ParserWithKey (memo k (parser p)) k

-- Mutually recursive higher order 
aSuf :: ParserWithKey T.Text T.Text -> ParserWithKey T.Text T.Text
aSuf p = smartMemoWithKey (Key (let k = (makeStableKey aSuf, key p) in trace (("a" :: String) <> show (hash k)) k)) $ ((<>) <$> bSuf p <*> string "a") <|> p

bSuf :: ParserWithKey T.Text T.Text -> ParserWithKey T.Text T.Text
bSuf p = smartMemoWithKey (Key (let k = (makeStableKey bSuf, key p) in trace (("b" :: String) <> show (hash k)) k)) $ (<>) <$> aSuf p <*> string "b"

cSuf :: ParserWithKey T.Text T.Text
cSuf = smartMemo $ aSuf (string "c")
