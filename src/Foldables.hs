module Foldables where

import           Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldr (\x r -> r || e == x) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\x r ->
  case r of
    Nothing -> Just x
    Just r
      | x < r -> Just x
      | otherwise -> Just r
    ) Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\x r ->
  case r of
    Nothing -> Just x
    Just r
      | x > r -> Just x
      | otherwise -> Just r
  )
  Nothing

null' :: Foldable t => t a -> Bool
null' xs =
  foldr (\_ _ -> Just ()) Nothing xs == Nothing

length' :: Foldable t => t a -> Int
length' xs = getSum $ foldMap (\_ -> Sum (1 :: Int)) xs

toList' :: Foldable t => t a -> [a]
toList' = foldMap (: [])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty

data Constant a b = Constant a deriving Show

instance Foldable (Constant a) where
  foldr _ s _ = s

data Two a b = Two a b deriving Show

instance Foldable (Two a) where
  foldr f s (Two a b) = f b s

data Three a b c = Three a b c deriving Show

instance Foldable (Three a b) where
  foldr f s (Three a b c) = f c s

data Three' a b = Three' a b b deriving Show

instance Foldable (Three' a) where
  foldr f s (Three' a b b') = f b' (f b s)

data Four a b = Four a b b b deriving Show

instance Foldable (Four a) where
  foldr f s (Four a b b' b'') = f b'' (f b' (f b s))

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
