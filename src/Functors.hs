module Functors where

import           Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                    (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- Pair

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a)  where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Identity Int)

  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Pair Int)

  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Two Int Int)
