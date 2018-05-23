module Parser.Assignment
       (
         lineParser
       , fileParser
       ) where

import Interpreter.Interpreter (Line (..))
import Parser.Base (Parser, rword')
import Parser.Expr (eqParser)
import Text.Megaparsec (many, (<|>))

newVarParser :: Parser Line
newVarParser = do
    rword' "mut"
    (ident, val) <- eqParser
    return $ NewVar ident val

assignParser :: Parser Line
assignParser = do
    (ident, val) <- eqParser
    return $ Assign ident val

lineParser :: Parser Line
lineParser = newVarParser <|> assignParser

fileParser :: Parser [Line]
fileParser = many lineParser
