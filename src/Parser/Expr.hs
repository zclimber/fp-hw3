module Parser.Expr
       (
         exprParser
       , eqParser
       ) where

import Control.Monad (void)
import Interpreter.Expr (Expr (..))
import Parser.Base (Parser, identifier, integer, lexeme, parens, rword', symbol')
import Text.Megaparsec (try, (<|>))
import Text.Megaparsec.Expr (Operator (..), makeExprParser)
import Interpreter.Types (Name)

eqParser :: Parser (Name, Expr)
eqParser = do
    ident <- identifier
    void (symbol' "=")
    val <- exprParser
    return (ident, val)

locParser :: Parser Expr
locParser = do
    rword' "let"
    (ident, val) <- eqParser
    rword' "in"
    right <- exprParser
    return $ Let ident val right

constParser :: Parser Expr
constParser = Lit <$> integer

varParser :: Parser Expr
varParser = Var <$> try identifier

tokenParser :: Parser Expr
tokenParser = lexeme constParser
   <|> varParser
   <|> parens (try locParser <|> exprParser)

operators :: [[Operator Parser Expr]]
operators = [[ InfixL (Mul <$ symbol' "*")
              , InfixL (Div <$ symbol' "/")]
            , [ InfixL (Add <$ symbol' "+")
              , InfixL (Sub <$ symbol' "-")]]

exprParser :: Parser Expr
exprParser = makeExprParser tokenParser operators
