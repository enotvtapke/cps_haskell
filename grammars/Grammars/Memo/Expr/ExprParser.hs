module Grammars.Memo.Expr.ExprParser (exprStart) where

import CPS.Parser.Memo (Parser, memo)
import CPS.Parser.Primitives
  ( MonadParser (eof, regex),
    oneOf,
    single,
  )
import Control.Applicative ((<|>))
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Grammars.Memo.Expr.Expr

f :: Parser T.Text F
f =
  memo $
    FVal . either undefined fst . decimal <$> regex "[0-9]+"
      <|> FExpr <$> (single '(' *> expr <* single ')')

term :: Parser T.Text Term
term =
  memo $
    TermVal <$> f
      <|> TermOp <$> term <*> (return <$> oneOf ['*', '/']) <*> f

expr :: Parser T.Text Expr
expr =
  memo $
    ExprOp <$> expr <*> (return <$> oneOf ['+', '-']) <*> term
      <|> ExprVal <$> term

exprStart :: Parser T.Text Expr
exprStart = expr <* eof
