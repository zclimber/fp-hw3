{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Interpreter.Assignment
       (
         newVar
       , assign
       ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), get, modify)
import qualified Data.Map.Strict as Map (insert, lookup)
import Interpreter.Types (InterpreteError (..), Name, State)

newVar :: ( MonadState State           m
          , MonadError InterpreteError m
          ) => Name -> Integer -> m ()
newVar name val = do
    st <- get
    case Map.lookup name st of
      Nothing -> modify $ Map.insert name val
      _       -> throwError $ DuplicateVariable name

assign :: ( MonadState State           m
          , MonadError InterpreteError m
          ) => Name -> Integer -> m ()
assign name val = do
    st <- get
    case Map.lookup name st of
      Just _ -> modify $ Map.insert name val
      _      -> throwError $ UnknownVariable name
