module Grammars.Memo.Expr.Expr (Expr (..), Term (..), F (..)) where

data Expr = ExprOp Expr String Term | ExprVal Term deriving (Eq)

data Term = TermOp Term String F | TermVal F deriving (Eq)

data F = FExpr Expr | FVal Int deriving (Eq)

instance Show Expr where
  show :: Expr -> String
  show (ExprOp e op t) = show e <> op <> show t
  show (ExprVal t) = show t

instance Show Term where
  show :: Term -> String
  show (TermOp t op ff) = show t <> op <> show ff
  show (TermVal ff) = show ff

instance Show F where
  show :: F -> String
  show (FExpr e) = "(" <> show e <> ")"
  show (FVal v) = show v
