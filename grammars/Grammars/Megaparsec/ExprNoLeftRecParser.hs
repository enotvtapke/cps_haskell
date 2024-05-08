module Grammars.Megaparsec.ExprNoLeftRecParser (exprStart) where

import Data.Text qualified as T
import Data.Text.Read (decimal)
import Grammars.Memo.Expr.Expr
import Data.Void (Void)
import Text.Megaparsec
    ( (<|>), oneOf, satisfy, single, some, Parsec, MonadParsec(..) )
import Data.Char (isDigit)

type Parser = Parsec Void T.Text

f :: Parser F
f =
    FVal . (either undefined fst . decimal . T.pack) <$> some (satisfy isDigit)
      <|> FExpr <$> (single '(' *> expr <* single ')')

term :: Parser Term
term =
  do
    x <- TermVal <$> f
    term' x

term' :: Term -> Parser Term
term' x =
    ( do
        op <- pure <$> oneOf ['*', '/']
        y <- f
        term' (TermOp x op y)
    )
    <|> pure x

expr :: Parser Expr
expr =
  do
    x <- ExprVal <$> term
    expr' x

expr' :: Expr -> Parser Expr
expr' x =
    ( do
        op <- pure <$> oneOf ['+', '-']
        y <- term
        expr' (ExprOp x op y)
    )
    <|> pure x

exprStart :: Parser Expr
exprStart = expr <* eof
