module DLists where

import           Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL (a:)

toList :: DList a -> [a]
toList (DL f) = f []

infixr `cons`
cons :: a -> DList a -> DList a
cons x (DL f) = DL ((x:) . f)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc (DL f) x = DL (f . (++[x]))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append (DL f) (DL g) = DL $ f . g
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456 ]
