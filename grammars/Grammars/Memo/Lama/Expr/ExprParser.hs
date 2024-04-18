module Grammars.Memo.Lama.Expr.ExprParser (basicStart) where

import CPS.Parser.Memo (Parser, memo)
import CPS.Parser.Primitives (MonadParser (chunk, eof, regex), single)
import CPS.Stream.Stream (ParserState)
import Control.Applicative (optional, (<|>))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Grammars.Memo.Lama.Expr.Expr (Attribute (..), Expr (..), LamaExpr, Primary (..))
import Prelude hiding (Left, Right)

type LamaParser t = Parser (ParserState T.Text) t

type Level x y s = (x -> x) -> x -> y -> s

data Assoc = Left | Right

left :: (x -> y -> s) -> (x -> x) -> x -> y -> s
left operation carry arg1 = operation (carry arg1)

right :: (x -> y -> s) -> (s -> s) -> x -> y -> s
right operation carry arg1 arg2 = carry $ operation arg1 arg2

assertValue :: Attribute -> v -> v
assertValue Ref _ = error "Reference expected"
assertValue Void vl = vl
assertValue _ vl = vl

expr :: [(Assoc, LamaParser (x -> x -> x))] -> LamaParser x -> LamaParser x
expr ops operand = inner (mapAssoc <$> ops) id
  where
    mapAssoc :: (Assoc, LamaParser (x -> x -> x)) -> LamaParser (Level x x x)
    mapAssoc (Left, op) = left <$> op
    mapAssoc (Right, op) = right <$> op
    inner [] c = c <$> operand
    inner (l : ls) c =
      do
        x <- inner ls id
        fromMaybe (c x)
          <$> optional
            ( do
                o <- l
                inner (l : ls) (o c x)
            )

primary :: LamaParser LamaExpr
primary =
  memo $
    (\x a -> assertValue a ((Primary . Const . either undefined fst . decimal) x)) <$> regex "-?[0-9]+"
      <|> (\x a -> assertValue a ((Primary . Var . T.unpack) x)) <$> regex "[a-z][a-z_A-Z0-9']*"
      <|> single '(' *> basic <* single ')'

binop :: String -> LamaParser (LamaExpr -> LamaExpr -> LamaExpr)
binop op = (\l r a -> assertValue a $ Binop op (l Val) (r Val)) <$ chunk (T.pack op)

basic :: LamaParser LamaExpr
basic =
  memo $
    expr
      [ (Left, binop "!!"),
        (Left, binop "&&"),
        (Left, binop "=="),
        (Left, binop "!="),
        (Left, binop "<"),
        (Left, binop ">"),
        (Left, binop "<="),
        (Left, binop ">="),
        (Left, binop "+"),
        (Left, binop "-"),
        (Left, binop "*"),
        (Left, binop "/"),
        (Left, binop "%")
      ]
      primary

basicStart :: Parser (ParserState T.Text) LamaExpr
basicStart = basic <* eof
