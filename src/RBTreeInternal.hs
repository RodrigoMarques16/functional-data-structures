module RBTreeInternal where

{- 
    Red black tree implementation based on the paper by Chris Okasaki
    https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf

    1) No red node has a red parent
    2) Every path from root to to an empty node 
       contains the same number of black nodes
-}

data RBColor = Red | Black deriving Show

data RBTree a = Empty
              | Branch RBColor a (RBTree a) (RBTree a)
              deriving Show

value :: RBTree a -> a
value (Branch _ x _ _) = x
value Empty            = undefined

left :: RBTree a -> RBTree a
left (Branch _ _ left _) = left
left Empty               = undefined

right :: RBTree a -> RBTree a
right (Branch _ _ _ right) = right
right Empty                = undefined

color :: RBTree a -> RBColor
color (Branch c _ _ _) = c
color Empty            = Red

empty :: Ord a => RBTree a
empty = Empty

isEmpty :: RBTree a -> Bool
isEmpty Empty = True
isEmpty _     = False

singleton :: a -> RBTree a
singleton x = Branch Black x Empty Empty

height :: RBTree a -> Int
height Empty                   = 0
height (Branch _ _ left right) = 1 + max (height left) (height right)

insert :: Ord a => a -> RBTree a -> RBTree a
insert x tree = Branch Black y a b
  where
    (Branch _ y a b) = ins tree
    ins Empty = Branch Red x Empty Empty
    ins (Branch c y a b) | x < y  = balance $ Branch c y (ins a) b
                         | x > y  = balance $ Branch c y a (ins b)
                         | x == y = Branch c y a b

balance :: RBTree a -> RBTree a
balance (Branch Black z (Branch Red y (Branch Red x a b) c) d) =
    Branch Red y (Branch Black x a b) (Branch Black z c d)
balance (Branch Black z (Branch Red x a (Branch Red y b c)) d) =
    Branch Red y (Branch Black x a b) (Branch Black z c d)
balance (Branch Black x a (Branch Red z (Branch Red y b c) d)) =
    Branch Red y (Branch Black x a b) (Branch Black z c d)
balance (Branch Black x a (Branch Red y b (Branch Red z c d))) =
    Branch Red y (Branch Black x a b) (Branch Black z c d)
balance t = t

fromList :: Ord a => [a] -> RBTree a
fromList = foldl (flip insert) Empty
