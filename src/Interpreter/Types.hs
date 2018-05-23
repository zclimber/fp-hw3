module Interpreter.Types
       (
         Name(..)
       , State
       , InterpreteError(..)
       ) where

import qualified Data.Map.Strict as Map (Map)

newtype Name = Name String
  deriving (Show, Eq, Ord)

type State = Map.Map Name Integer

data InterpreteError
  = DivisionByZero
  | UnknownVariable Name
  | DuplicateVariable Name
  | InvalidInput
  | LineInfo Int InterpreteError

instance Show InterpreteError where
  show DivisionByZero                = "Division by zero"
  show (UnknownVariable name)        = "Unknown variable " ++ show name
  show (DuplicateVariable name)      = "Variable " ++ show name ++ " is created second time"
  show InvalidInput                  = "Invalid integer input"
  show (LineInfo line err)           = case err of
    LineInfo _ inner -> show inner
    _                -> show err ++ " at line " ++ show line
