{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Parser.Parser
  (
  -- Cont(..),
  -- Parser,
    a,
    ccc,
    parse,
    accc
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

-- newtype C a = C {runC :: a -> State (C a, [a]) a}

newtype Cont a = Cont {runCont :: (a -> State ([a -> a], [a]) a) -> State ([a -> a], [a]) [a]}

-- newtype Cont a = Cont (ContT a [] a)

-- evalCont :: Cont a -> [a]
-- evalCont m = evalState (runCont m (\x -> (state $ \s -> (x, s)))) ([], [])

evalCont :: Cont a -> [a]
evalCont m = evalState (runCont m (\x -> state (x,))) ([], [])

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f m = Cont $ \k -> traverse (k . f) (evalCont m)

instance Applicative Cont where
  pure :: a -> Cont a
  pure x = Cont $ \k -> traverse k (pure x)
  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (<*>) f v = Cont $ \k -> traverse k (evalCont f <*> evalCont v)

instance Monad Cont where
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  (>>=) m f = Cont $ \k -> traverse k (evalCont m >>= (evalCont . f))
  return :: a -> Cont a
  return = pure

instance Alternative Cont where
  empty :: Cont a
  empty = Cont (const $ return empty)
  (<|>) :: Cont a -> Cont a -> Cont a
  (<|>) x y = Cont $ \k -> traverse k (evalCont x <|> evalCont y)

instance MonadPlus Cont

memoCont :: Cont a -> Cont a
memoCont m = Cont $ \k -> do
  (ks, rs) <- get
  -- return $ evalCont m
  if null ks then 
    do
      -- modify $ const (k : ks, rs)
      runCont m $ \t -> do
        (kss, rss) <- get
        modify $ const (kss, t : rss)
        return t

      return $ evalCont m 
  else return $ evalCont m

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
parse p s = evalCont (runStateT p s)

-- memo :: Int -> Parser s a -> Parser s a
-- memo key p = StateT $ \s -> ()
--   where 
--     m :: Map.HashMap s (Cont a)
--     m = empty

ccc :: Parser T.Text T.Text
ccc = (ccc >>= \c -> T.append c <$> term "c") <|> term "c"

accc = (term "c" >>= \c -> T.append c <$> term "c") <|> term "a" <|> term "a"

a :: [(T.Text, T.Text)]
a = parse ccc $ T.pack "ccc"