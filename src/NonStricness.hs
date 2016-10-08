{-# LANGUAGE BangPatterns #-}

module NonStricness where

import           Debug.Trace

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    False -> 0
    True -> 1

x = const 1 undefined
x' = (\_ -> 1) undefined
y = const undefined 1
y' = (\_ -> undefined) 1
y'' = undefined
z = flip const undefined 1
z' = flip (\x _ -> x) undefined 1
z'' = (\_ x -> x) undefined 1
z''' = 1
f = flip const 1 undefined
f' = flip (\x _ -> x) 1 undefined
f'' = (\_ x -> x) 1 undefined
f''' = undefined
g = const undefined undefined
g' = (\x _ -> x) undefined undefined
g'' = undefined
h = foldr const 'z' ['a'..'e']
h' = foldr (\x _ -> x) 'z' ['a'..'e']
h'' = 'a'
i = foldr (flip const) 'z' ['a'..'e']
i' = foldr (\_ s -> s) 'z' ['a'..'e']
i'' = 'z'

foo = trace "calling foo" 1
two = foo + foo

data List a
  = Nil
  | Cons !a !(List a)
  deriving Show

sTake :: Int -> List a -> List a
sTake n _ | n <= 0 = Nil
sTake n Nil = Nil
sTake n (Cons x xs) = Cons x (sTake (n - 1) xs)

twoEls = Cons 1 (Cons undefined Nil)
oneEl = sTake 1 twoEls
threeElements = Cons 2 twoEls
oneElT = sTake 1 threeElements

t = undefined
u = "blah"
main = print (snd (t, u))
