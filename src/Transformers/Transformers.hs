module Transformers where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT m) = EitherT $ (fmap . fmap) f m

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure (pure a)
  EitherT f <*> EitherT a = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT ma >>= f =
    EitherT $ do
      v <- ma
      case v of
        Left e -> return $ Left e
        Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither m =
  case m of
    Left e -> Right e
    Right a -> Left a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ fmap swapEither ma

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT amb) =
  amb >>= \ab ->
    case ab of
      Left a -> amc a
      Right b -> bmc b

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ pure (pure a)
  ReaderT fmab <*> ReaderT mab = ReaderT $ (<*>) <$> fmab <*> mab

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) =
    StateT $ \s -> fmap (\(a, s) -> (f a, s)) (sma s)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  StateT fsma <*> StateT sma =
    StateT $ \s -> do
      (a, s') <- sma s
      (f, s'') <- fsma s'
      return (f a, s'')
