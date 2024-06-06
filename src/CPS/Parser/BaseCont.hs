module CPS.Parser.BaseCont
  ( BaseParser,
    -- baseMemo,
    -- baseParse,
    -- baseSat,
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
import Data.Bifunctor (Bifunctor (first))
import Data.Dynamic (Dynamic (..), Typeable, fromDyn, toDyn)
import Data.HashMap.Lazy qualified as Map
import Data.Hashable (Hashable)
import Data.Typeable (typeOf)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Internal (ParsecT(..))
import Text.Megaparsec (runParserT')
import Text.Megaparsec qualified as M (State)

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

baseMemo1 :: (Typeable a, Hashable k, Eq k) => k -> Cont (ContState k) a -> Cont (ContState k) a
baseMemo1 key parser =
  Cont $ \continuation ->
    do
      -- modify $ Map.insertWith (\_ old -> old) key Map.empty
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
baseMemo key parser = StateT $ \state -> baseMemo1 (key, state) (runStateT parser state)
--   Cont $ \continuation ->
--     do
--       modify $ Map.insertWith (\_ old -> old) key Map.empty
--       entry <- gets $ \table -> Map.lookup state $ table Map.! key
--       case entry of
--         Nothing -> do
--           modify $ addNewEntry state $ MemoEntry [] [toDynContinuation continuation]
--           runCont
--             (runStateT parser state)
--             ( \result -> do
--                 modify (addResult state result)
--                 conts <- gets $ \table -> continuations $ (table Map.! key) Map.! state
--                 join <$> mapM (\cont -> fmap fromDynUnsafe <$> cont (first toDyn result)) conts
--             )
--         Just foundEntry -> do
--           modify (addContinuation state continuation)
--           join <$> mapM (continuation . first fromDynUnsafe) (results foundEntry)
--   where
--     toDynContinuation :: (Typeable r, Typeable a) => ((a, s) -> ContState k s [r]) -> (Dynamic, s) -> ContState k s [Dynamic]
--     toDynContinuation cont x = fmap toDyn <$> cont (first fromDynUnsafe x)
--     addNewEntry state entry table = Map.insert key (Map.insert state entry (table Map.! key)) table
--     addResult state res table = Map.insert key (Map.adjust (\e -> MemoEntry (first toDyn res : results e) (continuations e)) state (table Map.! key)) table
--     addContinuation state cont table = Map.insert key (Map.adjust (\e -> MemoEntry (results e) (toDynContinuation cont : continuations e)) state (table Map.! key)) table
--     fromDynUnsafe :: (Typeable a) => Dynamic -> a
--     fromDynUnsafe dynamic = fromDyn dynamic $ error ("Dynamic has invalid type.\nGot: " <> show (typeOf dynamic))

-- baseSat :: (s -> Bool) -> BaseParser k s ()
-- baseSat f = do
--   s <- get
--   guard (f s)

-- baseParse :: (Typeable s, Typeable t) => BaseParser k s t -> s -> [(t, s)]
-- baseParse p s = evalState idContState Map.empty
--   where
--     idContState = runCont (runStateT p s) (return . pure)

-- type Parser = Parsec Void Text

-- type Cc k = (Cont (ContState (k, State Text Void)))

type Parsec k e = ParsecT Void Text ((Cont (ContState (k, M.State Text Void))))

baseMemoPar :: (Typeable a, Hashable k, Eq k) => k -> Parsec k e a -> Parsec k e a
baseMemoPar key parser = let cc = ParsecT $ \state a b c d -> baseMemo1 (key, state) (let bb = (unParser parser) state a b c d in bb) in cc
