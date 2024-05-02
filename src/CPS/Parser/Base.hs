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

data MemoEntry k s a r = MemoEntry {results :: [(a, s)], continuations :: [(a, s) -> ContState k s [r]]}

type MemoTable k s = Map.HashMap k (Map.HashMap s (MemoEntry k s Dynamic Dynamic))

type ContState k s = State (MemoTable k s)

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
  pure a = Cont (\cont -> cont a)
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
      entry <- gets $ \table -> Map.lookup state $ table Map.! key
      case entry of
        Nothing -> do
          modify $
            addNewEntry state $
              MemoEntry [] [toDynamicContinuation continuation]
          runCont
            (runStateT parser state)
            ( \result -> do
                modify (addResult state result)
                conts <- gets $ \table -> continuations $ (table Map.! key) Map.! state
                join <$> mapM (\cont -> fmap fromDynamic <$> cont (first toDyn result)) conts
            )
        Just foundEntry -> do
          modify (addContinuation state continuation)
          join
            <$> mapM
              (continuation . first fromDynamic)
              (results foundEntry)
  where
    toDynamicContinuation :: (Typeable r, Typeable a) => ((a, s) -> ContState k s [r]) -> (Dynamic, s) -> ContState k s [Dynamic]
    toDynamicContinuation cont x = fmap toDyn <$> cont (first fromDynamic x)
    addNewEntry state entry table = Map.insert key (Map.insert state entry (table Map.! key)) table
    addResult state res table = Map.insert key (Map.adjust (\e -> MemoEntry (first toDyn res : results e) (continuations e)) state (table Map.! key)) table
    addContinuation state cont table = Map.insert key (Map.adjust (\e -> MemoEntry (results e) (toDynamicContinuation cont : continuations e)) state (table Map.! key)) table
    fromDynamic :: (Typeable a) => Dynamic -> a
    fromDynamic dynamic = fromDyn dynamic $ error ("Dynamic has invalid type.\nGot: " <> show (typeOf dynamic))

baseSat :: (s -> Bool) -> BaseParser k s ()
baseSat f = do
  s <- get
  guard (f s)

baseParse :: (Typeable s, Typeable t, Hashable s) => BaseParser k s t -> s -> [(t, s)]
baseParse p s = evalState idContState Map.empty
  where
    idContState = runCont (runStateT p s) (return . pure)
