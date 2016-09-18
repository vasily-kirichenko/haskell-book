module Applicatives where

import           Control.Applicative
import           Data.List                (elemIndex)
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

y' :: Maybe Integer
y' = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y' <*> z

x :: Maybe Int
x = elemIndex 3 [1..5]

y :: Maybe Int
y = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y

xs = [1..3]
ys = [4..6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x' <*> y'')

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

newtype Constant a b = Constant { getConstant :: a } deriving Show

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a) <*> (Constant b) = Constant $ a <> b

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

xx = const <$> Just "Hello" <*> pure "World"
yy =
  (,,,)
  <$> Just 90
  <*> Just 10
  <*> Just "Tierness"
  <*> Just [1, 2, 3]

-- Lows

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

-- ZipList

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

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

zip' :: List a -> List b -> List (a, b)
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (Cons x xs) (Cons y ys) = Cons (x, y) (zip' xs ys)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (<*>) fs as = flatMap (`fmap` as) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    elements [Nil, Cons a Nil]

instance Eq a => EqProp (List a) where (=-=) = eq

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ Cons a Nil
  ZipList' l <*> ZipList' l' =
    ZipList' (fmap (\(f, x) -> f x) (zip' l l'))

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x t) = Cons x (take' (n - 1) t)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

-- Variations of Either

data Sum' a b
  = First' a
  | Second' b
  deriving (Eq, Show)

data Validation e a
  = Error' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap _ (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

instance Applicative (Sum' a) where
  pure = Second'
  First' a <*> _ = First' a
  _ <*> First' a = First' a
  Second' f <*> Second' b = Second' $ f b

instance (Eq a, Eq b) => EqProp (Sum' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First' a, Second' b]

instance Functor (Validation e) where
  fmap _ (Error' e) = Error' e
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  Error' e <*> Error' e' = Error' $ e <> e'
  Error' e <*> _ = Error' e
  _ <*> Error' e = Error' e
  Success' f <*> Success' a = Success' $ f a

instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Error' a, Success' b]

main :: IO ()
main = do
  quickBatch $ monoid (ZipList [1 :: Sum Int])
  quickBatch $ applicative (Cons (1 :: Int, 2 :: Int, 3 :: Int) Nil)

  quickBatch $
    applicative (ZipList' (Cons (1 :: Int, 2 :: Int, 3 :: Int) Nil))

  quickBatch $
    applicative (First' (1, 2, 3) :: Sum' (Int, Int, Int) (Int, Int, Int))

  quickBatch $
    applicative (Error' ("a", "b", "c")
                  :: Validation (String, String, String)
                                (String, String, String))
