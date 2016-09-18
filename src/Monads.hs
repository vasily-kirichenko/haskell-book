module Monads where

import           Data.Monoid              hiding (First, Second, Sum)
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second x = Second $ f x

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= f = f b

-- Nope

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

-- PEither

data PEither b a
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap _ (Right' b) = Right' b

instance Applicative (PEither b) where
  pure = Left'
  Left' f <*> Left' a = Left' (f a)
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b

instance Monad (PEither b) where
  (Left' a) >>= f = f a
  (Right' b) >>= _ = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' a, Right' b]

instance (Eq a, Eq b) => EqProp (PEither a b) where (=-=) = eq

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monad (undefined :: Nope (Int, Int, Int))
  quickBatch $ monad (undefined :: PEither (Int, Int, Int)
                                           (Int, Int, Int))
  quickBatch $ monad (undefined :: Identity (Int, Int, Int))
