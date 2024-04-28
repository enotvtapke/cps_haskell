module CPS.Parser.Base
  ( BaseParser,
    baseMemo,
    baseParse,
    baseSat,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus, guard)
import Control.Monad.State
  ( MonadState (get),
    State,
    StateT (..),
    evalState,
    modify,
  )
import Data.Dynamic (Dynamic (..), Typeable, fromDyn, toDyn)
import Data.HashMap.Lazy qualified as Map
import Data.Hashable (Hashable)
import Data.Typeable (typeOf)
import GHC.Base (Alternative (empty), join)

type Table k s = Map.HashMap k (Map.HashMap s (Entry k s Dynamic))

type ContState k s = State (Table k s)

data Entry k s t = Entry {rs :: [t], ks :: [t -> ContState k s [t]]}

newtype Cont k s t = Cont {runCont :: forall r. (Typeable r) => (t -> ContState k s [r]) -> ContState k s [r]}

type BaseParser k s = StateT s (Cont k s)

instance Monad (Cont k s) where
  (>>=) :: Cont k s a -> (a -> Cont k s b) -> Cont k s b
  (>>=) c f = Cont (\k -> runCont c (\r -> runCont (f r) k))

instance Functor (Cont k s) where
  fmap :: (a -> b) -> Cont k s a -> Cont k s b
  fmap f m = Cont (\k -> runCont m (k . f))

instance Applicative (Cont k s) where
  pure :: a -> Cont k s a
  pure t = Cont (\k -> k t)
  (<*>) :: Cont k s (a -> b) -> Cont k s a -> Cont k s b
  (<*>) f m = Cont (\k -> runCont f (\r -> runCont (r <$> m) k))

instance Alternative (Cont k s) where
  empty :: Cont k s a
  empty = Cont (\_ -> return empty)
  (<|>) :: Cont k s a -> Cont k s a -> Cont k s a
  (<|>) a b =
    Cont
      ( \k -> do
          r1 <- runCont a k
          r2 <- runCont b k
          return $ r1 <|> r2
      )

instance MonadPlus (Cont k s)

baseMemo :: (Typeable s, Typeable t, Hashable k, Hashable s, Eq k, Eq s) => k -> BaseParser k s t -> BaseParser k s t
baseMemo key p = StateT (\s ->
    Cont (\k ->
        do
          modify (Map.insertWith (\_ old -> old) key Map.empty)
          table <- get
          let entry = Map.lookup s $ table Map.! key
          case entry of
            Nothing -> do
              modify (addNewEntry k s)
              runCont
                (runStateT p s)
                (\r -> do
                    modify (addR r s)
                    table2 <- get
                    let conts = ks ((table2 Map.! key) Map.! s)
                    join <$> mapM (\cont -> (fromDynOrError <$>) <$> cont (toDyn r)) conts
                )
            _ -> do
              modify (addK k s)
              table2 <- get
              let results = rs ((table2 Map.! key) Map.! s)
              join <$> mapM (k . fromDynOrError) results
      )
  )
  where
    kToDyn :: (Typeable r, Typeable t) => (t -> ContState k s [r]) -> Dynamic -> ContState k s [Dynamic]
    kToDyn k r = (toDyn <$>) <$> k (fromDynOrError r)
    addNewEntry k s oldMap = Map.insert key (Map.insert s (Entry [] [kToDyn k]) (oldMap Map.! key)) oldMap
    addR r s oldMap = Map.insert key (Map.adjust (\e -> Entry (toDyn r : rs e) (ks e)) s (oldMap Map.! key)) oldMap
    addK k s oldMap = Map.insert key (Map.adjust (\e -> Entry (rs e) (kToDyn k : ks e)) s (oldMap Map.! key)) oldMap
    fromDynOrError :: Typeable a => Dynamic -> a
    fromDynOrError d = fromDyn d $ error ("Dynamic has invalid type.\nGot: " <> show (typeOf d))

baseSat :: (s -> Bool) -> BaseParser k s ()
baseSat f = do
  s <- get
  guard (f s)

baseParse :: (Typeable s, Typeable t, Hashable s) => BaseParser k s t -> s -> [(t, s)]
baseParse p s = evalState idContState Map.empty
  where
    idContState = runCont (runStateT p s) (return . pure)
