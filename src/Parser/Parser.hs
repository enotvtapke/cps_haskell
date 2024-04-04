{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Parser.Parser
  (
  -- Cont(..),
  -- Parser,
    a,
    ccc,
    parse,
    accc,
    cc,
    evalCont,
    parse1
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

newtype C a = C {runC :: a -> State ([C a], [a]) a}

newtype Cont a = Cont {runCont :: C a -> State ([C a], [a]) [a]}

instance Show (Cont a) where
  show :: Cont a -> String
  show _ = "Cont"

-- newtype Cont a = Cont (ContT a [] a)

-- evalCont :: Cont a -> [a]
-- evalCont m = evalState (runCont m (\x -> (state $ \s -> (x, s)))) ([], [])

evalCont :: Cont a -> [a]
evalCont m = evalState (runCont m $ C (\x -> state (x,))) ([], [])

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f m = Cont $ \k -> traverse (runC k . f) (evalCont m)

instance Applicative Cont where
  pure :: a -> Cont a
  pure x = Cont $ \k -> traverse (runC k) (pure x)
  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (<*>) f v = Cont $ \k -> traverse (runC k) (evalCont f <*> evalCont v)

instance Monad Cont where
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  (>>=) m f = Cont $ \k -> traverse (runC k) (evalCont m >>= (evalCont . f))
  return :: a -> Cont a
  return = pure

instance Alternative Cont where
  empty :: Cont a
  empty = Cont (const $ return empty)
  (<|>) :: Cont a -> Cont a -> Cont a
  (<|>) x y = Cont $ \k -> traverse (runC k) (evalCont x <|> evalCont y)

instance MonadPlus Cont

memoCont :: Cont a -> Cont a
memoCont m = Cont $ \k -> do
  (ks, rs) <- get
  -- return $ evalCont m
  if null ks then
    do
      modify $ const (k : ks, rs)
      runCont m $ C $ \t -> do
        (kss, rss) <- get
        modify $ const (kss, t : rss)
        traverse_ (\kk -> runC kk t) kss
        return t
      -- return $ evalCont m 
  else
    do
      modify $ const (k : ks, rs)
      traverse (\r -> runC k r) rs
    -- return $ evalCont m

cc :: Cont String
cc = memoCont $ cc >> return "a"

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

newtype ParserState k s a = ParserState { runParserState :: State (Map.HashMap k (Map.HashMap s (Cont Dynamic))) (Cont a) }

evalParserState :: ParserState k s a -> Cont a
evalParserState m = evalState (runParserState m) Map.empty

instance Functor (ParserState k s) where
  fmap :: (a -> b) -> ParserState k s a -> ParserState k s b
  fmap f m = ParserState $ (f <$>) <$> runParserState m
  -- fmap f m = ParserState $ state (f <$> evalParserState m, )

instance Applicative (ParserState k s) where
  pure :: a -> ParserState k s a
  pure x = ParserState $ state (pure x, )
  (<*>) :: ParserState k s (a -> b) -> ParserState k s a -> ParserState k s b
  (<*>) f m = ParserState $ ((\x -> (x <*>)) <$> runParserState f) <*> runParserState m

instance Monad (ParserState k s) where
  (>>=) :: ParserState k s a -> (a -> ParserState k s b) -> ParserState k s b
  (>>=) m f = ParserState $ runParserState m >>= (\x -> let z = evalCont $ x >>= (\y -> return $ f y) in
    case z of
      [z1] -> runParserState z1
      _ -> state (empty,)
    )
  return :: a -> ParserState k s a
  return = pure

instance Alternative (ParserState k s) where
  empty :: ParserState k s a
  empty = ParserState $ state (empty, )
  (<|>) :: ParserState k s a -> ParserState k s a -> ParserState k s a
  (<|>) x y = ParserState $ do
    xx <- runParserState x
    yy <- runParserState y
    return $ xx <|> yy

instance MonadPlus (ParserState k s)

type Parser k s = StateT s (ParserState k s)
-- newtype MemoParser s a = MemoParser (StateT (State1 s a) Cont)

term1 :: T.Text -> Parser k T.Text T.Text
term1 t =
  StateT
    ( \s ->
        case T.stripPrefix t s of
          Just x -> return (t, x)
          Nothing -> empty
    )

term :: String -> Parser k T.Text T.Text
term = term1 . T.pack


-- evalParserState :: ParserState k s a -> Cont a
-- evalParserState m = evalState (runParserState m) Map.empty

parse :: Parser k s a -> s -> [(a, s)]
parse p s = evalCont (evalParserState (runStateT p s))

parse1 :: Parser k s a -> s -> Cont (a, s)
parse1 p s = evalParserState (runStateT p s)

memo :: (Hashable k, Eq k, Hashable s, Eq s, Typeable a, Typeable s) => k -> Parser k s a -> Parser k s a
memo key p = StateT $ \s -> ParserState $
  do
    modify (Map.insertWith (\_ old -> old) key Map.empty)
    keyToParser <- get
    keyToParser1 <- get
    let memoizedParser = keyToParser Map.! key
    let memoizedParser1 = keyToParser1 Map.! key
    case Map.lookup s memoizedParser of
      Nothing -> do
        let !memoizedCont = memoCont $ evalParserState $ runStateT p s
        -- let memoizedCont = memoCont (evalParserState $ evalStateT p s)
        modify (Map.adjust (Map.insert s (toDyn <$> memoizedCont)) key)
        return memoizedCont
      Just memoizedCont -> return $ flip fromDyn undefined <$> memoizedCont

    -- modify (Map.insertWith (\ _ old -> old) (Map.insert s (memoCont (evalParserState $ runStateT p s))) key)
    -- return $ (memoized Map.! key) Map.! s
    -- case memoParser of
    --   -- Nothing -> modify (Map.insert key Map.empty)
    --   Nothing -> undefined
    --   Just memoParserEntry -> do
    --     case Map.lookup s memoParserEntry of
    --       Nothing -> do
    --         let a = memoCont (evalParserState $ runStateT p s)
    --         modify (Map.adjust (Map.insert s (memoCont (evalParserState $ runStateT p s))) key)
    --         return a
    --       Just memoContEntry -> return memoContEntry
    -- return $ evalParserState (runStateT p s)

ccc :: Parser Int T.Text T.Text
ccc = memo 2 ((ccc >>= \c -> T.append c <$> term "c") <|> term "a")

accc :: Parser Int T.Text T.Text
accc = memo 1 ((term "c" >>= \c -> T.append c <$> term "c") <|> term "a" <|> term "a")

a :: [(T.Text, T.Text)]
a = parse ccc $ T.pack "ccc"