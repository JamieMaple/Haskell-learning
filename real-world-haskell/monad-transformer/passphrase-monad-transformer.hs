module PassphraseMonadTransformer where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Passphrase (isValid)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = MaybeT . return . return
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))
    m *> k = m >>= \_ -> k

instance (Monad m) => Monad (MaybeT m) where
    return = MaybeT . return . return
    x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                            Nothing -> return Nothing
                            Just value -> runMaybeT $ f value

instance Monad m => Alternative (MaybeT m) where
    empty = MaybeT $ return Nothing
    x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                            Nothing -> runMaybeT y
                            Just _ -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where
    mzero = empty
    mplus = (<|>)

-- usually use in `do` block to use
instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)

getPassphrase :: MaybeT IO String
getPassphrase = do s <- lift getLine
                   guard (isValid s) -- Alternative provide guard
                   return s

askPassphrase :: MaybeT IO ()
askPassphrase = do lift $ putStrLn "Insert your new passphrase"
                   value <- getPassphrase
                   lift $ putStrLn "Storing in database..."
-- guard
-- guard True = pure ()
-- guard False = empty

