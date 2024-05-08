module Grammars.Memo.Expr.ExprNoLeftRecParser (exprStart) where

import CPS.Parser.Base (BaseParser, DeterministicAlternative (..))
import CPS.Parser.Primitives
  ( MonadParser (eof, regex),
    oneOf,
    single,
  )
import CPS.Stream.Stream (ParserState (..))
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Grammars.Memo.Expr.Expr

type Parser s = BaseParser Int s

f :: Parser (ParserState T.Text) F
f =
  (FVal . (either undefined fst . decimal) <$> regex "[0-9]+")
    </> (FExpr <$> (single '(' *> expr <* single ')'))

term :: Parser (ParserState T.Text) Term
term =
  do
    x <- TermVal <$> f
    term' x

term' :: Term -> Parser (ParserState T.Text) Term
term' x =
  ( do
      op <- pure <$> oneOf ['*', '/']
      y <- f
      term' (TermOp x op y)
  )
    </> pure x

expr :: Parser (ParserState T.Text) Expr
expr =
  do
    x <- ExprVal <$> term
    expr' x

expr' :: Expr -> Parser (ParserState T.Text) Expr
expr' x =
  ( do
      op <- pure <$> oneOf ['+', '-']
      y <- term
      expr' (ExprOp x op y)
  )
    </> pure x

exprStart :: Parser (ParserState T.Text) Expr
exprStart = expr <* eof
