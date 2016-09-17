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

-- Three a b c

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

-- Three' a b b

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

-- Four a b c d

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

-- Four' a b = Four' a a a b

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Identity Int)

  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Pair Int)

  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Two Int Int)

  quickCheck $ \x -> functorIdentity (x :: Three Int Int String)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three String String Int)

  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three' String Int)

  quickCheck $ \x -> functorIdentity (x :: Four Int Int Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four String String String Int)

  quickCheck $ \x -> functorIdentity (x :: Four' Int Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four' String Int)
