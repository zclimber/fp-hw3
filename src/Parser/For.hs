module Parser.For
       (
         lineParser
       , fileParser
       ) where

import Interpreter.Interpreter (LineFor (..))
import Parser.Base (Parser, rword', curlyParens)
import Parser.Expr (exprParser)
import qualified Parser.IO as A (lineParser)
import Text.Megaparsec (many, (<|>))

forParser :: Parser LineFor
forParser = do
    rword' "for"
    from <- exprParser
    rword' "to"
    to <- exprParser
    acts <- curlyParens (many innerLineParser)
    return $ For from to acts

breakParser :: Parser LineFor
breakParser = do
    rword' "break"
    return Break

restParser :: Parser LineFor
restParser = do
    line <- A.lineParser
    return $ JustIO line

innerLineParser :: Parser LineFor
innerLineParser = lineParser <|> breakParser

lineParser :: Parser LineFor
lineParser = forParser <|> restParser

fileParser :: Parser [LineFor]
fileParser = many lineParser
