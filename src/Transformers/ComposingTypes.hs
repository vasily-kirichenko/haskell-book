{-# LANGUAGE InstanceSigs #-}

module ComposingTypes where

import           Control.Applicative

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure (pure a)
  Compose fgab <*> Compose fga =
    Compose $ (<*>) <$> fgab <*> fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap fm (Compose fga) = (foldMap . foldMap) fm fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse afb (Compose fga) = Compose <$> (traverse . traverse) afb fga
