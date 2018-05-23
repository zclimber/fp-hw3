{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Interpreter.IO
       (
         printVar
       , readVar
       ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState (..), get, modify)
import qualified Data.Map.Strict as Map (insert, lookup)
import Text.Read (readMaybe)
import Interpreter.Types

printVar :: (MonadIO m) => Integer -> m ()
printVar x = liftIO $ print x

readVar :: forall m . (MonadState State m, MonadError InterpreteError m, MonadIO m) => Name -> m ()
readVar name = do
    s <- liftIO getLine
    st <- get
    var <- case readMaybe s of
      Just x -> pure x
      _      ->  throwError InvalidInput
    case Map.lookup name st of
      Just _ -> modify $ Map.insert name var
      _      -> throwError $ UnknownVariable name
