module Semigroups where

import           Data.Semigroup
import           Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance (Arbitrary a, Semigroup a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- Two

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b, Semigroup a, Semigroup b) =>
  Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- Four

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    (Four x y z h) <> (Four x' y' z' h') =
      Four (x <> x') (y <> y') (z <> z') (h <> h')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

type FourAssoc a b c d =
  Four a b c d ->
  Four a b c d ->
  Four a b c d ->
  Bool

-- BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Or

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _ <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-- Combine

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) =
    Combine $ \a -> f a <> g a

-- Comp

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f. g

-- Validation

data Validation a b
  = Failure' a
  | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure' a <> Failure' a' = Failure' $ a <> a'
  Failure' a <> Success' _ = Failure' a
  Success' _ <> Failure' a = Failure' a
  Success' b <> Success' _ = Success' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [Failure' a, Success' b]

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

-- AccumulateBoth

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Failure' a) <> AccumulateBoth (Failure' a') =
    AccumulateBoth . Failure' $ a <> a'
  AccumulateBoth (Failure' a) <> AccumulateBoth (Success' _) =
    AccumulateBoth $ Failure' a
  AccumulateBoth (Success' _) <> AccumulateBoth (Failure' b) =
    AccumulateBoth $ Failure' b
  AccumulateBoth (Success' b) <> AccumulateBoth (Success' b') =
    AccumulateBoth . Success' $ b <> b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    v <- elements [Failure' a, Success' b]
    return $ AccumulateBoth v

type AccumulateBothAssoc a b =
  AccumulateBoth a b ->
  AccumulateBoth a b ->
  AccumulateBoth a b ->
  Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc [Int])
  quickCheck (semigroupAssoc :: TwoAssoc [Int] [Double])
  quickCheck (semigroupAssoc :: FourAssoc [Int] [Double] [String] [Int])
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc [Int] [Int])
  quickCheck (semigroupAssoc :: ValidationAssoc [String] [Int])
  quickCheck (semigroupAssoc :: AccumulateBothAssoc [String] [Int])
