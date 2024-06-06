module CPS.Parser.Base
  ( BaseParser,
    baseMemo,
    baseParse,
    baseSat,
    DeterministicAlternative(..),
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
import Data.Dynamic (Dynamic (..), Typeable, fromDyn, toDyn)
import Data.HashMap.Lazy qualified as Map
import Data.Hashable (Hashable)
import Data.Typeable (typeOf)

data MemoEntry k a r = MemoEntry {results :: [a], continuations :: [a -> ContState k [r]]}

type MemoTable k = Map.HashMap k (MemoEntry k Dynamic Dynamic)

type ContState k = State (MemoTable k)

newtype Cont m a = Cont {runCont :: forall r. (Typeable r) => (a -> m [r]) -> m [r]}

type BaseParser k s = StateT s (Cont (ContState (k, s)))

instance Monad m => Monad (Cont m) where
  (>>=) :: Cont m a -> (a -> Cont m b) -> Cont m b
  (>>=) m f = Cont (\cont -> runCont m (\r -> runCont (f r) cont))

instance Functor m => Functor (Cont m) where
  fmap :: (a -> b) -> Cont m a -> Cont m b
  fmap f m = Cont (\cont -> runCont m (cont . f))

instance Applicative m => Applicative (Cont m) where
  pure :: a -> Cont m a
  pure a = Cont (\cont -> cont a)
  (<*>) :: Cont m (a -> b) -> Cont m a -> Cont m b
  (<*>) f m = Cont (\cont -> runCont f (\r -> runCont (r <$> m) cont))

instance (Monad m) => Alternative (Cont m) where
  empty :: Cont m a
  empty = Cont (\_ -> return empty)
  (<|>) :: Cont m a -> Cont m a -> Cont m a
  (<|>) l r =
    Cont
      ( \k ->
        do
          leftResults <- runCont l k
          rightResults <- runCont r k
          return $ leftResults <|> rightResults
      )

instance (Monad m) => MonadPlus (Cont m)

infixl 3 </>

class Alternative f => DeterministicAlternative f where
  (</>) :: f a -> f a -> f a

instance Monad m => DeterministicAlternative (Cont m) where
  (</>) :: Cont m a -> Cont m a -> Cont m a
  (</>) l r =
    Cont
      ( \k -> do
          leftResults <- runCont l k
          case leftResults of
            [] -> runCont r k
            _ -> return leftResults
      )

instance DeterministicAlternative (BaseParser k s) where
  (</>) :: BaseParser k s a -> BaseParser k s a -> BaseParser k s a
  StateT m </> StateT n = StateT $ \s -> m s </> n s

memoCont :: (Typeable a, Hashable k, Eq k) => k -> Cont (ContState k) a -> Cont (ContState k) a
memoCont key parser =
  Cont $ \continuation ->
    do
      entry <- gets $ \table -> Map.lookup key table
      case entry of
        Nothing -> do
          modify $ addNewEntry $ MemoEntry [] [toDynContinuation continuation]
          runCont
            parser
            ( \result -> do
                modify (addResult result)
                conts <- gets $ \table -> continuations $ table Map.! key
                join <$> mapM (\cont -> fmap fromDynUnsafe <$> cont (toDyn result)) conts
            )
        Just foundEntry -> do
          modify (addContinuation continuation)
          join <$> mapM (continuation . fromDynUnsafe) (results foundEntry)
  where
    toDynContinuation :: (Typeable r, Typeable a) => (a -> ContState k [r]) -> Dynamic -> ContState k [Dynamic]
    toDynContinuation cont x = fmap toDyn <$> cont (fromDynUnsafe x)
    fromDynUnsafe :: (Typeable a) => Dynamic -> a
    fromDynUnsafe dynamic = fromDyn dynamic $ error ("Dynamic has invalid type.\nGot: " <> show (typeOf dynamic))
    addNewEntry = Map.insert key
    addResult res = Map.adjust (\e -> MemoEntry (toDyn res : results e) (continuations e)) key
    addContinuation cont = Map.adjust (\e -> MemoEntry (results e) (toDynContinuation cont : continuations e)) key

baseMemo :: (Typeable a, Hashable k, Hashable s, Eq k, Eq s, Typeable s) => k -> BaseParser k s a -> BaseParser k s a
baseMemo key parser = StateT $ \state -> memoCont (key, state) (runStateT parser state)

baseSat :: (s -> Bool) -> BaseParser k s ()
baseSat f = do
  s <- get
  guard (f s)

baseParse :: (Typeable s, Typeable t) => BaseParser k s t -> s -> [(t, s)]
baseParse p s = evalState idContState Map.empty
  where
    idContState = runCont (runStateT p s) (return . pure)
