{-# LANGUAGE DerivingVia #-}

module Parser.Parser
  (
  -- Cont(..),
  -- Parser,
    a,
    ccc,
    parse,
    ccc1
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (Monad, MonadPlus (mzero))
import Control.Monad.Cont (ContT (..), runContT)
import Control.Monad.State
import Data.Monoid qualified as GHC.Types
import Data.Text qualified as T
import GHC.Base (Alternative (empty), join)
-- import Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

-- import Control.Monad.Trans.Cont ()

newtype Cont a = Cont {runCont :: (a -> a) -> [a]}

-- newtype Cont a = Cont (ContT a [] a)

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f m = Cont $ \k -> k . f <$> runCont m id

instance Applicative Cont where
  pure :: a -> Cont a
  pure x = Cont $ \k -> pure $ k x
  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (<*>) f v = Cont $ \k -> k <$> (runCont f id <*> runCont v id)

instance Monad Cont where
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  (>>=) m f = Cont $ \k -> k <$> (runCont m id >>= (\x -> runCont (f x) id))
  return :: a -> Cont a
  return = pure

instance Alternative Cont where
  empty :: Cont a
  empty = Cont (const empty)
  (<|>) :: Cont a -> Cont a -> Cont a
  (<|>) x y = Cont $ \k -> k <$> (runCont x id <|> runCont y id)

instance MonadPlus Cont

-- newtype St a = St (StateT String [] a) deriving Monad

-- a :: ContT GHC.Types.Any [] GHC.Types.Any -> Cont GHC.Types.Any
-- a = return (\x -> x) >>= \x -> return x

-- instance Monad (ContT String []) where
-- mzero = return $ \x -> []

-- type Res s a = (s a)

-- newtype MemCont a = MemCont { runMemCont :: State ([a -> a], [a]) (Cont a) }

-- data MemoCont a = MemoCont
--   { cont :: Cont a,
--     ks :: [a -> a],
--     rs :: [a]
--   }

-- memoCont :: Cont a -> MemoCont a
-- memoCont k = 

-- data State1 s a = State1 s (Map.HashMap Int (Map.HashMap s (Cont a)) )

type Parser s = StateT s Cont
-- newtype MemoParser s a = MemoParser (StateT (State1 s a) Cont)

term1 :: T.Text -> Parser T.Text T.Text
term1 t =
  StateT
    ( \s ->
        case T.stripPrefix t s of
          Just x -> return (t, x)
          Nothing -> empty
    )

term :: String -> Parser T.Text T.Text
term = term1 . T.pack

parse :: Parser s a -> s -> [(a, s)]
parse p s = runCont (runStateT p s) id

-- memo :: Int -> Parser s a -> Parser s a
-- memo key p = StateT $ \s -> ()
--   where 
--     m :: Map.HashMap s (Cont a)
--     m = empty

ccc :: Parser T.Text T.Text
ccc = (ccc >>= \c -> T.append c <$> term "c") <|> term "c"

ccc1 = (term "c") <|> term "c"

a :: [(T.Text, T.Text)]
a = parse ccc $ T.pack "ccc"