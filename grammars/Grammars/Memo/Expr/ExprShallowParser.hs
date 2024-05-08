module Grammars.Memo.Expr.ExprShallowParser (stmtsStart) where

import CPS.Parser.Memo (Key (..), Parser, getOrMakeKey, makeStableKey, memo, memoWithKey)
import CPS.Parser.Primitives
  ( MonadParser (chunk, eof, regex, satisfy),
    oneOf,
    single,
  )
import CPS.Stream.Stream (ParserState (..))
import Control.Applicative (Alternative (many), (<|>))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.State
  ( StateT (..),
    evalStateT,
    modify,
  )
import Data.Char (isSpace)
import Data.Data (Typeable)
import Data.HashMap.Lazy qualified as M
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Prelude hiding (seq)
import CPS.Parser.Base ((</>))

newtype Error = Error {unError :: String} deriving (Show, Eq)

type Res = StateT (M.HashMap String Int) (Either Error) Int

tok :: Parser (ParserState T.Text) a -> Parser (ParserState T.Text) a
tok p = p <* many (satisfy isSpace)

binop :: Char -> Int -> Int -> Int
binop '*' = (*)
binop '/' = div
binop '+' = (+)
binop '-' = (-)
binop s = error $ "Unknown operation " ++ show s

varName :: Parser (ParserState T.Text) String
varName = T.unpack <$> tok (regex "[A-z]+")

f :: Parser (ParserState T.Text) Res
f =
  memo $
    return . either undefined fst . decimal <$> tok (regex "[0-9]+")
      <|> (tok (single '(') *> expr <* tok (single ')'))
      <|> ( \name ->
              StateT
                ( \s ->
                    case M.lookup name s of
                      Nothing -> throwError $ 
                        Error $ "Undefined variable " ++ name
                      Just x -> return (x, s)
                )
          )
        <$> varName

term :: Parser (ParserState T.Text) Res
term =
  memo $
    f
      <|> do
        t <- term
        op <- binop <$> tok (oneOf ['*', '/'])
        ff <- f
        return $
          do
            tVal <- t
            op tVal <$> ff

expr :: Parser (ParserState T.Text) Res
expr =
  memo $
    term
      <|> do
        e <- expr
        op <- binop <$> tok (oneOf ['+', '-'])
        t <- term
        return $
          do
            eVal <- e
            op eVal <$> t

monadSeq ::
  (Typeable m, Monad m, Typeable a, Typeable b) =>
  Parser (ParserState T.Text) (m a) ->
  Parser (ParserState T.Text) (m b) ->
  Parser (ParserState T.Text) (m b)
monadSeq p1 p2 =
  memoWithKey (Key (monadSeqLabel, getOrMakeKey p1, getOrMakeKey p2)) $
    (\v1 _ v2 -> v1 >> v2) <$> p1 <*> tok (single ';') <*> p2
  where
    monadSeqLabel = makeStableKey monadSeqLabel

varDefinition :: Parser (ParserState T.Text) Res
varDefinition =
  memo $ do
    name <- varName
    _ <- tok $ chunk $ T.pack ":="
    e <- expr
    return $ do
      eVal <- e
      modify (M.insert name eVal)
      return eVal

stmts :: Parser (ParserState T.Text) Res
stmts = monadSeq stmts stmts </> (varDefinition <|> expr)

stmtsStart :: Parser (ParserState T.Text) (Either Error Int)
stmtsStart = (`evalStateT` M.empty) <$> stmts <* eof
