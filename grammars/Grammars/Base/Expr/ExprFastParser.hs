module Grammars.Base.Expr.ExprFastParser (exprStart) where

import CPS.Parser.Base
import CPS.Parser.Primitives
import Control.Applicative ((<|>))
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Grammars.Memo.Expr.Expr
import CPS.Stream.Stream (ParserState)

f :: BaseParser Int (ParserState T.Text) F
f =
  baseMemo 3 $
    FVal . either undefined fst . decimal <$> regex "[0-9]+"
      <|> FExpr <$> (single '(' *> expr <* single ')')

term :: BaseParser Int (ParserState T.Text) Term
term =
  baseMemo 2 $
    TermVal <$> f
      <|> TermOp <$> term <*> (return <$> oneOf ['*', '/']) <*> f

expr :: BaseParser Int (ParserState T.Text) Expr
expr =
  baseMemo 1 $
    ExprOp <$> expr <*> (return <$> oneOf ['+', '-']) <*> term
      <|> ExprVal <$> term

exprStart :: BaseParser Int (ParserState T.Text) Expr
exprStart = expr <* eof
