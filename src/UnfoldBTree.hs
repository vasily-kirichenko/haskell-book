module UnfoldBTree where

  data BTree a
    = Leaf
    | Node (BTree a) a (BTree a)
    deriving (Eq, Ord, Show)

  unfold :: (a -> Maybe (a, b, a)) -> a -> BTree b
  unfold f x =
    case f x of
      Nothing -> Leaf
      Just (l, b, r) ->
        Node (unfold f l) b (unfold f r)

  treeBuilder :: Integer -> BTree Integer
  treeBuilder size =
    unfold (\n ->
      if n == size then Nothing
      else Just (n + 1, n, n + 1)
      )
      0
