# `Monad` transformer

``` haskell
-- examples
newtype State s a = State { runState :: s -> (a, s) }
StateT :: (s -> m (a, s)) -> State T s m a
-- add mutable state to an underlying monad

newtype Writer w a = Writer { runWriter :: (a, w) }
newtype WriterT w (m :: *-> *) a = WriterT { runWriterT : m (a, w) }
-- make it possible to write data when stacked on top of another monad

-- MonadReader
class (Monad m) => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r ) -> m a -> m a
-- m represent the immutable state that reader monad carries around
-- `Reader r` monad is an instance of the `MonadReader` class
```



