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

-- type Cont k s m a = a -> Table k s a m
-- data Entry k s a = Entry { rs :: [a], cs :: [Cont k s a a]}
-- newtype Table k s m a = Table { unTable :: State (Map.HashMap k (Map.HashMap s (Entry k s m))) [a] }
-- newtype Parser k s a = Parser { runParser :: s -> Cont k s (Dynamic, s) (a, s) -> Table k s (Dynamic, s) (a, s) }

type Cont k s m b a = a -> Table k s m b
data Entry k s a b m = Entry { rs :: [a], cs :: [Cont k s m b a]}
type Table k s m b = State (Map.HashMap k (Map.HashMap s (Entry k s m b m))) [b]
newtype Parser k s a = Parser { runParser :: forall b.
  s -> Cont k s (Dynamic, s) (b, s) (a, s) -> Table k s (Dynamic, s) (b, s)
}


-- toDyn :: Cont k s (a, s) (b, s) (a, s) -> Cont k s (Dynamic, s) (b, s) (a, s)
-- toDyn c a = StateT $ \s -> c a

-- bind :: Cont k s m a -> (a -> Cont k s m b) -> Cont k s m b
-- -- bind m f = \c -> let gg = (\x -> let ff = (f x) in ff c) in m gg
-- bind m f = \c -> m (\x -> (f x) c)

-- instance Functor (Parser k s) where
--   fmap :: (a -> b) -> Parser k s a -> Parser k s b
--   fmap f p = Parser (\s c -> let pp = (runParser p) s in (let xx = (c) . f in pp xx))

instance Monad (Parser k s) where
  (>>=) :: Parser k s a -> (a -> Parser k s b) -> Parser k s b
  (>>=) (Parser p) f = Parser (\s c -> p s (\(a, s1) -> runParser (f a) s1 c))

instance Functor (Parser k s) where
  fmap :: (a -> b) -> Parser k s a -> Parser k s b
  fmap f (Parser p) = Parser $ \s c -> p s $ \(a, s1) -> c (f a, s1)

instance Applicative (Parser k s) where
  pure :: a -> Parser k s a
  pure x = Parser $ \s c -> c (x, s)
  (<*>) :: Parser k s (a -> b) -> Parser k s a -> Parser k s b
  (<*>) (Parser p1) (Parser p2) = Parser $ \s c ->
    p1 s $ \(f, s1) ->
    p2 s1 $ \(x, s2) ->
    c (f x, s2)

-- memo :: (Hashable k, Eq k, Hashable s, Eq s, Typeable a, Typeable s) => k -> Parser k s a -> Parser k s a
-- memo key p = Parser $ \s c ->
--   do
--     modify (Map.insertWith (\_ old -> old) key Map.empty)
--     keyToParser <- get
--     let memoizedParser = keyToParser Map.! key
--     let entry = Map.lookup s memoizedParser
--     case entry of
--       Nothing -> do
--         let mem = (Entry [] [c])
--         let ff = (Map.adjust (\x -> Map.insert s mem x) key )
--         modify ff
--         return undefined
--       Just (Entry rs cs) -> undefined
--     return undefined
    -- case Map.lookup s memoizedParser of
    --   Nothing -> do
    --     let !memoizedCont = memoCont $ evalParserState $ runStateT p s
    --     -- let memoizedCont = memoCont (evalParserState $ evalStateT p s)
    --     modify (Map.adjust (Map.insert s (toDyn <$> memoizedCont)) key)
    --     return memoizedCont
    --   Just memoizedCont -> return $ flip fromDyn undefined <$> memoizedCont

-- memoCont :: Cont k s m b a -> Cont k s m b a
-- memoCont m k = do
--   (ks, rs) <- get
--   -- return $ evalCont m
--   if null ks then
--     do
--       modify $ const (k : ks, rs)
--       runCont m $ C $ \t -> do
--         (kss, rss) <- get
--         modify $ const (kss, t : rss)
--         traverse_ (\kk -> runC kk t) kss
--         return t
--       -- return $ evalCont m 
--   else
--     do
--       modify $ const (k : ks, rs)
--       traverse (\r -> runC k r) rs
--     -- return $ evalCont m
