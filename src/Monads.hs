module Monads where

import           Control.Monad            (join)
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

-- List

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (<*>) fs as = flatMap (`fmap` as) fs

instance Monad List where
  Nil >>= _ = Nil
  (Cons a l) >>= f = f a `append` (l >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    elements [Nil, Cons a Nil]

instance Eq a => EqProp (List a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monad (undefined :: Nope (Int, Int, Int))
  quickBatch $ monad (undefined :: PEither (Int, Int, Int)
                                           (Int, Int, Int))
  quickBatch $ monad (undefined :: Identity (Int, Int, Int))
  quickBatch $ monad (undefined :: List (Int, Int, Int))
