{-# LANGUAGE DeriveAnyClass #-}

module Grammars.Memo.Lama.Expr.Expr
  ( Expr (..),
    Primary (..),
    Attribute (..),
    LamaExpr,
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Expr = Binop String Expr Expr | Primary Primary deriving (Generic, NFData)

data Primary = Const Int | Var String deriving (Generic, NFData)

instance Show Primary where
  show :: Primary -> String
  show (Const c) = "Const (" <> show c <> ")"
  show (Var x) = "Var (" <> show x <> ")"

instance Show Expr where
  show :: Expr -> String
  show (Binop op l r) = "Binop (" <> show op <> ", " <> show l <> ", " <> show r <> ")"
  show (Primary pr) = show pr

data Attribute = Ref | Void | Val

type LamaExpr = Attribute -> Expr
