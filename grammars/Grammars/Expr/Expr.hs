module Grammars.Expr.Expr (Expr (..), Term (..), F (..), exprStart) where

import CPS.Parser.Core
import CPS.Parser.Primitives
import Control.Applicative ((<|>))
import Data.Text qualified as T
import Data.Text.Read (decimal)

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

f :: Parser Int T.Text F
f =
  memo 3 $
    FVal . either undefined fst . decimal <$> regex "[0-9]+"
      <|> FExpr <$> (single '(' *> expr <* single ')')

term :: Parser Int T.Text Term
term =
  memo 2 $
    TermVal <$> f
      <|> TermOp <$> term <*> (return <$> oneOf ['*', '/']) <*> f

expr :: Parser Int T.Text Expr
expr =
  memo 1 $
    ExprOp <$> expr <*> (return <$> oneOf ['+', '-']) <*> term
      <|> ExprVal <$> term

exprStart :: Parser Int T.Text Expr
exprStart = expr <* eof
