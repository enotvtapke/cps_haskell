module CPS.Parser.Base
  ( BaseParser,
    baseMemo,
    baseParse,
    baseSat,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus, guard, join)
import Control.Monad.State
  ( MonadState (get),
    State,
    StateT (..),
    evalState,
    modify,
  )
import Control.Monad.State.Lazy (gets)
import Data.Bifunctor (Bifunctor (first))
import Data.Dynamic (Dynamic (..), Typeable, fromDyn, toDyn)
import Data.HashMap.Lazy qualified as Map
import Data.Hashable (Hashable)
import Data.Typeable (typeOf)

type MemoTable k s = Map.HashMap k (Map.HashMap s (MemoEntry k s Dynamic Dynamic))

type ContState k s = State (MemoTable k s)

data MemoEntry k s a r = MemoEntry {results :: [(a, s)], continuations :: [(a, s) -> ContState k s [r]]}

newtype Cont k s a = Cont {runCont :: forall r. (Typeable r) => (a -> ContState k s [r]) -> ContState k s [r]}

type BaseParser k s = StateT s (Cont k s)

instance Monad (Cont k s) where
  (>>=) :: Cont k s a -> (a -> Cont k s b) -> Cont k s b
  (>>=) m f = Cont (\cont -> runCont m (\r -> runCont (f r) cont))

instance Functor (Cont k s) where
  fmap :: (a -> b) -> Cont k s a -> Cont k s b
  fmap f m = Cont (\cont -> runCont m (cont . f))

instance Applicative (Cont k s) where
  pure :: a -> Cont k s a
  pure t = Cont (\cont -> cont t)
  (<*>) :: Cont k s (a -> b) -> Cont k s a -> Cont k s b
  (<*>) f m = Cont (\cont -> runCont f (\r -> runCont (r <$> m) cont))

instance Alternative (Cont k s) where
  empty :: Cont k s a
  empty = Cont (\_ -> return empty)
  (<|>) :: Cont k s a -> Cont k s a -> Cont k s a
  (<|>) l r =
    Cont
      ( \k -> do
          leftResults <- runCont l k
          rightResults <- runCont r k
          return $ leftResults <|> rightResults
      )

instance MonadPlus (Cont k s)

baseMemo :: (Typeable a, Hashable k, Hashable s, Eq k, Eq s) => k -> BaseParser k s a -> BaseParser k s a
baseMemo key parser = StateT $ \state ->
  Cont $ \continuation ->
    do
      modify (Map.insertWith (\_ old -> old) key Map.empty)
      entry <- gets (\table -> Map.lookup state $ table Map.! key)
      case entry of
        Nothing -> do
          modify (addNewEntry state (MemoEntry [] [toDynamicContinuation continuation]))
          runCont
            (runStateT parser state)
            ( \result -> do
                modify (addResult state result)
                conts <- gets (\table -> continuations $ (table Map.! key) Map.! state)
                join <$> mapM (\cont -> fmap fromDynamicOrError <$> cont (first toDyn result)) conts
            )
        Just foundEntry -> do
          modify (addContinuation state continuation)
          join <$> mapM (continuation . first fromDynamicOrError) (results foundEntry)
  where
    toDynamicContinuation :: (Typeable r, Typeable a) => ((a, s) -> ContState k s [r]) -> (Dynamic, s) -> ContState k s [Dynamic]
    toDynamicContinuation cont x = fmap toDyn <$> cont (first fromDynamicOrError x)
    addNewEntry state entry table = Map.insert key (Map.insert state entry (table Map.! key)) table
    addResult state res table = Map.insert key (Map.adjust (\e -> MemoEntry (first toDyn res : results e) (continuations e)) state (table Map.! key)) table
    addContinuation state cont table = Map.insert key (Map.adjust (\e -> MemoEntry (results e) (toDynamicContinuation cont : continuations e)) state (table Map.! key)) table
    fromDynamicOrError :: (Typeable a) => Dynamic -> a
    fromDynamicOrError dynamic = fromDyn dynamic $ error ("Dynamic has invalid type.\nGot: " <> show (typeOf dynamic))

-- {-# NOINLINE kToDyn #-}
-- {-# NOINLINE addNewEntry #-}
-- {-# NOINLINE addR #-}
-- {-# NOINLINE addK #-}
-- {-# NOINLINE fromDynOrError #-}
-- {-# SCC kToDyn #-}

-- kToDyn :: (Typeable r, Typeable t) => (t -> ContState k s [r]) -> Dynamic -> ContState k s [Dynamic]
-- {-# SCC kToDyn #-}
-- kToDyn k r = (toDyn <$>) <$> {-# SCC kkkkId #-} k (fromDynOrError r)

-- addR :: (Hashable k1, Hashable k2, Typeable a) => a -> k2 -> k1 -> Map.HashMap k1 (Map.HashMap k2 (Entry k3 s Dynamic)) -> Map.HashMap k1 (Map.HashMap k2 (Entry k3 s Dynamic))
-- {-# SCC addR #-}
-- addR r s key oldMap = Map.insert key (Map.adjust (\e -> Entry (toDyn r : rs e) (ks e)) s (oldMap Map.! key)) oldMap

-- addNewEntry :: (Hashable k1, Hashable k2, Typeable r, Typeable t) => (t -> ContState k3 s [r]) -> k2 -> k1 -> Map.HashMap k1 (Map.HashMap k2 (Entry k3 s Dynamic)) -> Map.HashMap k1 (Map.HashMap k2 (Entry k3 s Dynamic))
-- {-# SCC addNewEntry #-}
-- addNewEntry k s key oldMap = Map.insert key (Map.insert s (Entry [] [kToDyn k]) (oldMap Map.! key)) oldMap

-- addK :: (Hashable k1, Hashable k2, Typeable r, Typeable t) => (t -> ContState k3 s [r]) -> k2 -> k1 -> Map.HashMap k1 (Map.HashMap k2 (Entry k3 s Dynamic)) -> Map.HashMap k1 (Map.HashMap k2 (Entry k3 s Dynamic))
-- {-# SCC addK #-}
-- addK k s key oldMap = Map.insert key (Map.adjust (\e -> Entry (rs e) (kToDyn k : ks e)) s (oldMap Map.! key)) oldMap

-- fromDynOrError :: Typeable a => Dynamic -> a
-- {-# SCC fromDynOrError #-}
-- fromDynOrError d = fromDyn d $ error ("Dynamic has invalid type.\nGot: " <> show (typeOf d))

baseSat :: (s -> Bool) -> BaseParser k s ()
baseSat f = do
  s <- get
  guard (f s)

baseParse :: (Typeable s, Typeable t, Hashable s) => BaseParser k s t -> s -> [(t, s)]
baseParse p s = evalState idContState Map.empty
  where
    idContState = runCont (runStateT p s) (return . pure)
