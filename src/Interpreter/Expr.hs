{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Interpreter.Expr
       (
         Expr(..)
       , eval
       ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), asks, local)
import qualified Data.Map.Strict as Map (insert, lookup)
import Interpreter.Types (InterpreteError (..), Name, State)

data Expr
  = Lit Integer
  | Var Name
  | Let Name Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

eval :: ( MonadReader State          m
        , MonadError InterpreteError m
        ) => Expr -> m Integer
eval (Lit a) = return a
eval (Var name) = do
    val <- asks (Map.lookup name)
    case val of
      Just int -> return int
      _        -> throwError $ UnknownVariable name
eval (Let name a b) = do
    val <- eval a
    var <- asks (Map.lookup name)
    case var of
      Nothing -> local (Map.insert name val) (eval b)
      _       -> throwError $ DuplicateVariable name
eval (Add f s) = (+) <$> eval f <*> eval s
eval (Sub f s) = (-) <$> eval f <*> eval s
eval (Mul f s) = (*) <$> eval f <*> eval s
eval (Div f s) = do
    b <- eval s
    if b == 0
      then throwError DivisionByZero
      else div <$> eval f <*> pure b
