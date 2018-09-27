module Identity where

--import Control.Monad
import Control.Monad.Trans (MonadTrans, lift)
--import Control.Applicative

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where
    fmap f m = do
        val <- m
        return $ f val

instance Applicative Identity where
    pure = Identity
    mf <*> m = do
        f <- mf
        val <- m
        return $ f val

instance Monad Identity where
    return a = Identity a
    m >>= k = k (runIdentity m)

newtype IdentityT m a = IdentityT { runIdentityT :: m (Identity a) }

mapIdentityT :: (Monad m, Monad n) => (m (Identity a) -> n (Identity b)) -> IdentityT m a -> IdentityT n b
mapIdentityT f = IdentityT . f . runIdentityT

instance Functor f => Functor (IdentityT f) where
    fmap f m = IdentityT $ (fmap $ fmap f) (runIdentityT m)

instance Monad f => Applicative (IdentityT f) where
    pure = IdentityT . return . return
    mf <*> mv = IdentityT $ do
        f <- runIdentityT mf
        v <- runIdentityT mv
        return . mapIdentity f $ v
        where mapIdentity (Identity f) (Identity a) = Identity . f $ a

instance Monad f => Monad (IdentityT f) where
    return = IdentityT . return . return
    m >>= f = IdentityT $ do
        v <- runIdentityT m
        runIdentityT . mapIdentity f $ v
        where mapIdentity f (Identity a) = f a

instance MonadTrans IdentityT where
    lift = IdentityT . (fmap Identity)
    --lift m = IdentityT $ do
        --v <- m
        --return $ Identity v


