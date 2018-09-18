module State where

import Control.Monad.State

newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }

