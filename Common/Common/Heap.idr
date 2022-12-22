module Common.Heap

import Data.List as L

export
data Heap a = Empty
            | Node a (Heap a) (Heap a)

export
empty : Heap a
empty = Empty

rank : Heap a -> Nat
rank Empty = 0
rank (Node _ _ r) = rank r + 1

export
merge : Ord a => Heap a -> Heap a -> Heap a
merge a b =
  case a of
    Empty         => b
    Node ax al ar =>
      case b of
        Empty       => a
        Node bx _ _ =>
          if ax > bx
            then merge b a
            else let ar' = merge ar b
                 in if rank ar' > rank al
                      then Node ax ar' al
                      else Node ax al  ar'

export
singleton : a -> Heap a
singleton x = Node x Empty Empty

export
insert : Ord a => Heap a -> a -> Heap a
insert h x = merge h (Node x Empty Empty)

export
total
deleteMin : Ord a => Heap a -> Maybe (a, Heap a)
deleteMin Empty        = Nothing
deleteMin (Node x l r) = Just (x, merge l r)

export
fromList : Ord a => List a -> Heap a
fromList = foldl insert Empty

export
toList : Ord a => Heap a -> List a
toList = go 
    where
    go : Heap a -> List a
    go        Empty = []
    go (Node x l r) = x :: L.merge (go l) (go r)

export
(Ord a, Show a) => Show (Heap a) where
    show = show . Common.Heap.toList
