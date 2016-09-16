module MaybeEither where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (h:_) = Just h

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes =
  foldr
    (\m acc ->
      case m of
        Just x -> x : acc
        Nothing -> acc)
    []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms =
  case catMaybes ms of
    [] -> Nothing
    xs -> Just xs

lefts' :: [Either a b] -> [a]
lefts' =
  foldr
    (\m acc ->
      case m of
        Left x -> x : acc
        Right _ -> acc
    )
    []

rights' :: [Either a b] -> [b]
rights' =
  foldr
    (\m acc ->
      case m of
        Right x -> x : acc
        Left _ -> acc
    )
    []

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers =
  foldr
    (\m (ls, rs) ->
      case m of
        Left x -> (x:ls, rs)
        Right x -> (ls, x:rs)
    )
    ([], [])

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe f m =
  case m of
    Right x -> Just $ f x
    _ -> Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' lf rf m =
  case m of
    Left x -> lf x
    Right x -> rf x

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Just (a, b) -> a : myUnfoldr f b
    Nothing -> []

myIterate' :: (a -> a) -> a -> [a]
myIterate' f = myUnfoldr (\x -> Just(x, f x))
