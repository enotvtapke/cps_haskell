module CPS.Stream.MemoFun
  ( memoEq,
    memoStable,
  )
where

import Data.HashMap.Lazy qualified as Map
import Data.Hashable (Hashable)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe ( unsafePerformIO )
import System.Mem.StableName ( StableName, makeStableName )

memoEq :: (Eq a, Hashable a) => (a -> b) -> a -> b
memoEq f = unsafePerformIO (do tref <- newIORef Map.empty; return (applyEq f tref))
  where
    applyEq :: (Eq a, Hashable a) => (a -> b) -> IORef (Map.HashMap a b) -> a -> b
    applyEq g tref arg =
      unsafePerformIO
        ( do
            tbl <- readIORef tref
            case Map.lookup arg tbl of
              Just result -> return result
              Nothing -> do
                let res = g arg
                let tbl' = Map.insert arg res tbl
                writeIORef tref tbl'
                return res
        )

memoStable :: (a -> b) -> a -> b
memoStable f =
  unsafePerformIO
    ( do
        tref <- newIORef Map.empty
        return (applyStable f tref)
    )
  where
    applyStable :: (a -> b) -> IORef (Map.HashMap (StableName a) b) -> a -> b
    applyStable g tref arg =
      unsafePerformIO
        ( do
            tbl <- readIORef tref
            sn <- makeStableName arg
            let lkp = Map.lookup sn tbl
            case lkp of
              Just result -> return result
              Nothing -> do
                let res = g arg
                let tbl' = Map.insert sn res tbl
                writeIORef tref tbl'
                return res
        )