module Internal
  ( module Internal.Name
  , module Internal.Fresh
  , liftError)
  where

import Control.Monad.Error

import Internal.Name
import Internal.Fresh

liftError :: (Error e, MonadError e m) => Either e a -> m a 
liftError = either throwError return
