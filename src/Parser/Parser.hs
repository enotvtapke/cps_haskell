{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Parser.Parser
  (
  -- Cont(..),
  -- Parser,

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
-- import qualified Data.HashMap.Strict as Map
import Data.Foldable (traverse_)
import qualified Data.HashMap.Lazy as Map
import Data.Hashable
import Data.Dynamic

-- import Control.Monad.Trans.Cont ()

type St k s = Map.HashMap k (Map.HashMap s (Entry k s Dynamic))
-- type Sta k s = State (St k s) 
-- data Entry k s t = Entry { rs :: [t], cs :: [t -> St k s -> ([t], (St k s))]}
data Entry k s t = Entry { rs :: [t], cs :: [t -> (St k s -> ([t], (St k s)))]}
newtype Cont k s t = Cont { run :: forall r. (t -> (St k s -> ([r], St k s))) -> St k s -> ([r], St k s) }
type Parser k s = StateT s (Cont k s)

instance Monad (Cont k s) where
  (>>=) :: Cont k s a -> (a -> Cont k s b) -> Cont k s b
  (>>=) c f = Cont (\k s -> run c (\r ss -> run (f r) k ss) s)

instance Functor (Cont k s) where
  fmap :: (a -> b) -> Cont k s a -> Cont k s b
  fmap f m = Cont (\k s -> run m (\r s1 -> k (f r) s1) s)

instance Applicative (Cont k s) where
  pure :: a -> Cont k s a
  pure t = Cont (\k s -> k t s)
  (<*>) :: Cont k s (a -> b) -> Cont k s a -> Cont k s b
  (<*>) f m = Cont (\k s -> run f (\r s1 -> run (r <$> m) k s1) s)

instance Alternative (Cont k s) where
  empty :: Cont k s a
  empty = Cont (\_ s -> ([], s))
  (<|>) :: Cont k s a -> Cont k s a -> Cont k s a
  (<|>) a b = Cont (\k s -> do
      let r1 = run a k s
      let r2 = run b k (snd r1)
      (fst r1 <|> fst r2, snd r2)
    )

instance MonadPlus (Cont k s) where

memo :: k -> Parser k s t -> Parser k s t
memo key p = StateT (\s -> 
    Cont (\k table ->
        do
          
          return undefined
      )
  )

-- type Result = forall r.


-- type Cont a = ContT [a] (State (Entry a a))
-- data Entry a b = Entry { rs :: [a], cs :: [a -> (State (Entry a a) b)]}
-- -- newtype Table k s m a = Table { unTable :: State (Map.HashMap k (Map.HashMap s (Entry k s m))) [a] }
-- type Parser k s = StateT s (State (Map.HashMap k (Map.HashMap s (Cont Dynamic Dynamic))))

-- memoCont :: Cont a b -> Cont a b
-- memoCont m = ContT $ \k -> do
--   Entry ks rs <- get
--   return undefined
--   -- return $ evalCont m
--   if null ks then
--     do
--       modify $ const $ Entry (k : ks) rs
--       runContT m $ \t -> do
--         (kss, rss) <- get
--         modify $ const (kss, t : rss)
--         traverse_ (\kk -> kk t) kss
--         return [t]
--       -- return $ evalCont m 
--   else
--     do
--       modify $ const (k : ks, rs)
--       traverse (\r -> runC k r) rs
