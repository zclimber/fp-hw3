module Parser.Base
     (
       Parser
     , identifier
     , parens
     , symbol'
     , rword'
     , integer
     , lexeme
     , curlyParens
     ) where

import qualified Data.Text as T (Text, pack, unpack)
import Data.Void (Void)
import Interpreter.Types (Name(..))
import Text.Megaparsec (Parsec, between, empty, many, notFollowedBy, try)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space, symbol)

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

symbol' :: String -> Parser T.Text
symbol' x = symbol $ T.pack x

parens :: Parser a -> Parser a
parens = between (symbol' "(") (symbol' ")")

curlyParens :: Parser a -> Parser a
curlyParens = between (symbol' "{") (symbol' "}")

integer :: Parser Integer
integer = lexeme L.decimal

rword :: T.Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rword' :: String -> Parser ()
rword' w = rword $ T.pack w

rws :: [T.Text] -- list of reserved words
rws = map T.pack ["let","in","mut","for", "to", "break"]

identifier :: Parser Name
identifier = (lexeme . try) ((T.pack <$> p) >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check :: T.Text -> Parser Name
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return . Name $ T.unpack x
