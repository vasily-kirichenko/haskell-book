module MonadTranses where


import           Control.Monad
import           Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma
