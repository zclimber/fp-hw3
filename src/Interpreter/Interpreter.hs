{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Interpreter.Interpreter
       (
         Line(..)
       , LineIO(..)
       , LineFor(..)
       , interpret
       , interpretIO
       , interpretFor
       ) where

import Control.Monad.Cont
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadState (..))
import Interpreter.Assignment (assign, newVar)
import Interpreter.Expr (Expr(..), eval)
import Interpreter.IO (printVar, readVar)
import Interpreter.Types

data Line
  = NewVar Name Expr
  | Assign Name Expr
  deriving (Show)

data LineIO
  = JustLine Line
  | Read Name
  | Write Expr
  deriving (Show)

data LineFor
  = JustIO LineIO
  | For Expr Expr [LineFor]
  | Break
  deriving (Show)

lineCount :: LineFor -> Int
lineCount (For _ _ body) = (+ 1) $ sum $ map lineCount body
lineCount _              = 1

interpretLineFor :: ( MonadState State m
                    , MonadError InterpreteError m
                    , MonadIO m
                    , MonadCont m
                    ) => LineFor -> (() -> m ()) -> Int -> m()
interpretLineFor (JustIO line) _ _ = interpretLineIO line
interpretLineFor Break exit _ = exit ()
interpretLineFor (For from to body) _ i = do
    f <- get >>= runReaderT (eval from)
    t <- get >>= runReaderT (eval to)
    let diff = t - f
    when (diff <= 0) $ return ()
    callCC $ \exit -> do
        let bodyInt = zip body $ map (+ (i + 1)) $ scanl (+) 0 $ map lineCount body
        let cyc = replicate (fromIntegral diff) (interpretLinesFor bodyInt exit)
        foldr (<=<) return cyc ()


interpretLinesFor :: ( MonadState State m
                     , MonadError InterpreteError m
                     , MonadIO m
                     , MonadCont m
                     ) => [(LineFor, Int)] -> (() -> m ()) -> () -> m()
interpretLinesFor [] _ _ = return ()
interpretLinesFor ((x, i):xs) exit _ = do
    catchError (interpretLineFor x exit i) (throwError . LineInfo (i + 1))
    interpretLinesFor xs exit ()

interpretLine :: ( MonadState State           m
                 , MonadError InterpreteError m
                 ) => Line -> m ()
interpretLine (NewVar name expr) =
    get >>= runReaderT (eval expr) >>= newVar name
interpretLine (Assign name expr) =
    get >>= runReaderT (eval expr) >>= assign name

interpretLineIO :: ( MonadState State           m
                   , MonadError InterpreteError m
                   , MonadIO                    m
                   ) => LineIO -> m ()
interpretLineIO (JustLine line) = interpretLine line
interpretLineIO (Read name) = readVar name
interpretLineIO (Write expr) =
    get >>= runReaderT (eval expr) >>= printVar

interpret :: forall m .
            ( MonadState State           m
            , MonadError InterpreteError m
            ) => [Line] -> m ()
interpret list = interpret' $ zip list [1..]
    where
      interpret' :: [(Line, Int)] -> m()
      interpret' [] = pure ()
      interpret' ((x, i):xs) = do
        catchError (interpretLine x) (throwError . LineInfo i)
        interpret' xs

interpretIO :: forall m .
            ( MonadState State           m
            , MonadError InterpreteError m
            , MonadIO                    m
            ) => [LineIO] -> m ()
interpretIO list = interpret' $ zip list [1..]
    where
      interpret' :: [(LineIO, Int)] -> m()
      interpret' [] = pure ()
      interpret' ((x, i):xs) = do
        catchError (interpretLineIO x) (throwError . LineInfo i)
        interpret' xs

interpretFor :: forall m .
            ( MonadState State           m
            , MonadError InterpreteError m
            , MonadIO                    m
            , MonadCont                  m
            ) => [LineFor] -> m ()
interpretFor list = do
    let numberedList = zip list $ scanl (+) 0 $ map lineCount list
    interpretLinesFor numberedList return ()
