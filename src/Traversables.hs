{-# LANGUAGE FlexibleInstances #-}

module Traversables where

import           Data.Monoid              ((<>))
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f s (Identity a) = f a s

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- Constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ s _ = s

instance Traversable (Constant a) where
  traverse _ (Constant a) = Constant <$> pure a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

-- Optional

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ s Nada = s
  foldr f s (Yep a) = f a s

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [return Nada, return $ Yep a]

instance Eq a => EqProp (Optional a) where (=-=) = eq

-- List

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldr _ s Nil = s
  foldr f s (Cons a l) = foldr f (f a s) l

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a l) = Cons <$> f a <*> traverse f l

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    oneof [return Nil, return $ Cons a Nil]

instance Eq a => EqProp (List a) where (=-=) = eq

-- Three

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldr f s (Three _ _ c) = f c s

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary =
      Three
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- Three'

data Three' a b = Three' a b b deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldr f s (Three' _ b b') = f b (f b' s)

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary =
    Three'
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- S

data S n a = S (n a) a deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldr f s (S n a) = foldr f (f a s) n

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

instance Arbitrary a => Arbitrary (S Maybe a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    oneof [ return $ S Nothing a
          , return $ S (Just a') a ]

instance Eq a => EqProp (S Maybe a) where (=-=) = eq

-- Tree

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) =
    Node
    <$> traverse f l
    <*> f a
    <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    elements [ Empty, Leaf a, Node l a r ]

instance Eq a => EqProp (Tree a) where (=-=) = eq

main :: IO ()
main = do
    quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: Constant String (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: List (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: Three Int Int (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: Three' Int (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: S Maybe (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
