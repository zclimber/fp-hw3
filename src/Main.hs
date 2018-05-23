module Main where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, evalStateT)
import qualified Data.Map.Strict as Map (empty)
import qualified Data.Text as T (pack)
import Interpreter.Interpreter (LineFor, interpretFor)
import Parser.For (fileParser)
import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (parseErrorPretty)
import Interpreter.Types (InterpreteError, State)
import Control.Monad.Cont

type InterpreterStack = StateT State (ExceptT InterpreteError (ContT () IO)) ()

interpretRun :: [LineFor] -> InterpreterStack
interpretRun = interpretFor

main :: IO ()
main = do
    args <- getArgs
    file <- readFile $ head args
    let parsed = parse fileParser (head args) $ T.pack file
    case parsed of
      Left er -> putStr $ parseErrorPretty er
      Right l -> (`runContT` id) $ do
        result <- runExceptT (evalStateT (interpretRun l) Map.empty)
        case result of
          Left er -> return $ print er
          _       -> return $ putStr ""
