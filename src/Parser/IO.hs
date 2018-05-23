module Parser.IO
       (
         lineParser
       , fileParser
       ) where

import Control.Monad (void)
import Interpreter.Interpreter (LineIO (..))
import qualified Parser.Assignment as A (lineParser)
import Parser.Base (Parser, identifier, symbol')
import Parser.Expr (exprParser)
import Text.Megaparsec (many, (<|>))

writeParser :: Parser LineIO
writeParser = do
    void (symbol' "<")
    val <- exprParser
    return $ Write val

readParser :: Parser LineIO
readParser = do
    void (symbol' ">")
    ident <- identifier
    return $ Read ident

restParser :: Parser LineIO
restParser = do
    line <- A.lineParser
    return $ JustLine line

lineParser :: Parser LineIO
lineParser = writeParser <|> readParser <|> restParser

fileParser :: Parser [LineIO]
fileParser = many lineParser
