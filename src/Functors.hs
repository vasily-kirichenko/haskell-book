{-# LANGUAGE FlexibleInstances #-}

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

-- Possibly

data Possibly a
  = LolNone
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNone = LolNone
  fmap f (Yeppers a) = Yeppers $ f a

-- Sum

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

-- Company

data Company a b c
  = DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (DeepBlue a c) = DeepBlue a (f c)
  fmap _ (Something b) = Something b

-- More

data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Quant

data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- K

data K a b = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

-- EvilGoateeConst

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- LiftIfOut

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut $ fmap g fa

-- Parappa

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa f g) = DaWrappa (fmap h f) (fmap h g)

-- IgnoreOne

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething f g) = IgnoringSomething f (fmap h g)

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
