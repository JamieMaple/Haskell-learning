module State where

import Control.Monad.State (State, MonadState, get, put)

newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }

--instance (Monad m) => Monad (StateT s m) where
    --return a = StateT $ \s -> return (a,s)
    --(StateT x) >>= f = StateT $ \s -> do
        --(v,s') <- x s
        --runStateT (f v) s'

--instance (Monad m) => MonadState s (StateT s m) where
    --get = StateT $ \s -> return (s,s)
    --put = StateT $ \_ -> return ((),s)

--instance (MonadPlus m) => MonadPlus (StateT s m) where
    --mzero = StateT $ \_ -> mzero
    --(StateT x1) `mplus` (StateT x2) = StateT $ \s -> (x1 s) `mplus` (x2 s)

--instance MonadTrans (StateT s) where
    --lift c = StateT $ \s -> c >>= (\x -> return (x,s))

--state :: MonadState s m => (s -> (a,s)) -> m a
--

-- metion that
-- MaybeT (State s) not same with StateT s Maybe
-- 1st like (Just a, s)
-- 2nd like Just (a, s)


