{-# LANGUAGE InstanceSigs #-}

module Readers where

import           Control.Applicative (liftA2)
import           Data.Char

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupledd :: String -> (String, String)
tupledd = do
  x <- cap
  y <- rev
  return (x, y)

tupledb :: String -> (String, String)
tupledb =
  cap >>= \x ->
  rev >>= \y ->
  return (x, y)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName    :: DogName
  , dogsAddress :: Address
} deriving (Eq, Show)

kot :: Person
kot =
  Person (HumanName "kot")
         (DogName "duke")
         (Address "Streat 1st, 2nd")

she :: Person
she =
  Person (HumanName "she")
         (DogName "gray")
         (Address "No Streat, 3rd")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addr <- address
  return $ Dog name addr

instance Monad (Reader r) where
  return = pure
  Reader ra >>= aRb =
    Reader $ \r -> runReader (aRb . ra $ r) r

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- Reader dogName
  addr <- Reader address
  return $ Dog name addr
