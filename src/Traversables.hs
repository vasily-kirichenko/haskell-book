module Traversables where

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

main :: IO ()
main = do
    quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: Constant String (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
    quickBatch $ traversable (undefined :: List (Int, Int, [Int]))
