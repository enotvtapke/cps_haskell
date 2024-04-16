module Grammars.Memo.Expr.ExprNoLeftRecParser (exprStart) where

import CPS.Parser.Memo (Parser, memo)
import CPS.Parser.Primitives
  ( MonadParser (eof, regex),
    oneOf,
    single,
  )
import CPS.Stream.Stream (ParserState (..))
import Control.Applicative ((<|>))
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Grammars.Memo.Expr.Expr

f :: Parser (ParserState T.Text) F
f =
  memo $
    FVal . (either undefined fst . decimal) <$> regex "[0-9]+"
      <|> FExpr <$> (single '(' *> expr <* single ')')

term :: Parser (ParserState T.Text) Term
term =
  memo $ do
    x <- TermVal <$> f
    term' x

term' :: Term -> Parser (ParserState T.Text) Term
term' x =
  memo $
    pure x <|> do
      op <- pure <$> oneOf ['*', '/']
      y <- f
      term' (TermOp x op y)

expr :: Parser (ParserState T.Text) Expr
expr =
  memo $ do
    x <- ExprVal <$> term
    expr' x

expr' :: Expr -> Parser (ParserState T.Text) Expr
expr' x =
  memo $
    pure x <|> do
      op <- pure <$> oneOf ['+', '-']
      y <- term
      expr' (ExprOp x op y)

exprStart :: Parser (ParserState T.Text) Expr
exprStart = expr <* eof
