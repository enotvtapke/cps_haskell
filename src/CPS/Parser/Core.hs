module CPS.Parser.Core
  (
    Parser,
    memo,
    _parse,
    _sat
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus, guard)
import Control.Monad.State
    ( modify, evalState, MonadState(get), State, StateT(..) )
import GHC.Base (Alternative (empty), join)
import Data.HashMap.Lazy qualified as Map
import Data.Hashable ( Hashable )
import Data.Dynamic ( toDyn, Typeable, fromDyn, Dynamic )

type Table k s = Map.HashMap k (Map.HashMap s (Entry k s Dynamic))
type ContState k s = State (Table k s)
data Entry k s t = Entry { rs :: [t], ks :: [t -> ContState k s [t]]}
newtype Cont k s t = Cont { run :: forall r. (Typeable r) => (t -> ContState k s [r]) -> ContState k s [r] }
type Parser k s = StateT s (Cont k s)

instance Monad (Cont k s) where
  (>>=) :: Cont k s a -> (a -> Cont k s b) -> Cont k s b
  (>>=) c f = Cont (\k -> run c (\r -> run (f r) k))

instance Functor (Cont k s) where
  fmap :: (a -> b) -> Cont k s a -> Cont k s b
  fmap f m = Cont (\k -> run m (k . f))

instance Applicative (Cont k s) where
  pure :: a -> Cont k s a
  pure t = Cont (\k -> k t)
  (<*>) :: Cont k s (a -> b) -> Cont k s a -> Cont k s b
  (<*>) f m = Cont (\k -> run f (\r -> run (r <$> m) k))

instance Alternative (Cont k s) where
  empty :: Cont k s a
  empty = Cont (\_ -> return [])
  (<|>) :: Cont k s a -> Cont k s a -> Cont k s a
  (<|>) a b = Cont (\k -> do
      r1 <- run a k
      r2 <- run b k
      return $ r1 <|> r2
    )

instance MonadPlus (Cont k s) where

memo :: (Typeable s, Typeable t, Hashable k, Hashable s) => k -> Parser k s t -> Parser k s t
memo key p = StateT (\s ->
    Cont (\k ->
        do
          modify (Map.insertWith (\_ old -> old) key Map.empty)
          table <- get
          let entry = Map.lookup s $ table Map.! key
          case entry of
            Nothing -> do
              modify (addNewEntry k s)
              run
                (runStateT p s)
                (\r -> do
                    modify (addR r s)
                    table2 <- get
                    let conts = ks ((table2 Map.! key) Map.! s)
                    join <$> mapM (\cont -> (\ds -> (`fromDyn` undefined) <$> ds) <$> cont (toDyn r)) conts
                )
            _ -> do
              modify (addK k s)
              table2 <- get
              let results = rs ((table2 Map.! key) Map.! s)
              join <$> mapM (\res -> k (fromDyn res undefined)) results
      )
  )
  where
    kToDyn :: (Typeable r, Typeable t) => (t -> ContState k s [r]) -> Dynamic -> ContState k s [Dynamic]
    kToDyn k r = (toDyn <$>) <$> k (fromDyn r undefined)
    addNewEntry k s oldMap = Map.insert key (Map.insert s (Entry [] [kToDyn k]) (oldMap Map.! key)) oldMap
    addR r s oldMap = Map.insert key (Map.adjust (\e -> Entry (toDyn r : rs e) (ks e)) s (oldMap Map.! key)) oldMap
    addK k s oldMap = Map.insert key (Map.adjust (\e -> Entry (rs e) (kToDyn k : ks e)) s (oldMap Map.! key)) oldMap

_sat :: (s -> Bool) -> Parser k s ()
_sat f = do
  s <- get
  guard (f s)

_parse :: (Typeable s, Typeable t, Hashable s) => Parser k s t -> s -> [(t, s)]
_parse p s = evalState idContState Map.empty
  where
    idContState = run (runStateT p s) (\t -> return [t])
