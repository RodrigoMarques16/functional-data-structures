module RBTree where

-- 1) no red node has a red child
-- 2) every path from root to to an empty node 
--    contains the same number of black nodes

data RBColor = Red | Black deriving Show
data RBTree a = Empty | Branch RBColor a (RBTree a) (RBTree a) deriving Show

value :: RBTree a -> a
value Empty = undefined
value (Branch _ x _ _) = x

left :: RBTree a -> RBTree a
left Empty = undefined
left (Branch _ _ left _) = left

right :: RBTree a -> RBTree a
right Empty = undefined
right (Branch _ _ _ right) = right

color :: RBTree a -> RBColor
color Empty = Red
color (Branch c _ _ _) = c

empty :: Ord a => RBTree a
empty = Empty

isEmpty :: RBTree a -> Bool
isEmpty Empty = True
isEmpty _     = False

singleton :: a -> RBTree a
singleton x = Branch Red x Empty Empty

height :: RBTree a -> Int
height Empty = 0
height (Branch _ _ left right ) = 1 + max (height left) (height right)

width :: RBTree a -> Int
width Empty                  = 2
width (Branch _ _ Empty Empty) = 3
width (Branch _ _ left  Empty) = 2 + width left
width (Branch _ _ Empty right) = 2 + width right
width (Branch _ _ left  right) = width right + width right

-- okasaki
insert :: Ord a => a -> RBTree a -> RBTree a
insert x tree = Branch Black y a b
  where
    (Branch _ y a b) = ins tree
    ins Empty = Branch Red x Empty Empty
    ins (Branch c y a b) 
        | x < y     = balance $ Branch c y (ins a) b
        | x > y     = balance $ Branch c y a (ins b)
        | otherwise = tree


balance :: RBTree a -> RBTree a
balance (Branch Black z (Branch Red y (Branch Red x a b) c) d) =
    Branch Red y (Branch Black x a b) (Branch Black z c d)
balance (Branch Black z (Branch Red x a (Branch Red y b c)) d) =
    Branch Red y (Branch Black x a b) (Branch Black z c d)
balance (Branch Black x a (Branch Red z (Branch Red y b c) d)) =
    Branch Red y (Branch Black x a b) (Branch Black z c d)
balance (Branch Black x a (Branch Red y b (Branch Red z c d))) =
    Branch Red y (Branch Black x a b) (Branch Black z c d)
-- balance (Branch Black topV (Branch Red midV (Branch Red botV botL botR) midR) topR)
--     = Branch Red
--              midV
--              (Branch Black botV botL botR)
--              (Branch Black topV midR topR)
-- balance (Branch Black topV (Branch Red midV midL (Branch Red botV botL botR)) topR)
--     = Branch Red
--              botV
--              (Branch Black midV midL botL)
--              (Branch Black topV botR topR)
-- balance (Branch Black topV topL (Branch Red midV (Branch Red botV botL botR) midR))
--     = Branch Red
--              botV
--              (Branch Black topV topL botL)
--              (Branch Black midV botR midR)
-- balance (Branch Black topV topL (Branch Red midV midL (Branch Red botV botL botR)))
--     = Branch Red
--              midV
--              (Branch Black topV topL midL)
--              (Branch Black botV botL botR)
balance t = t

fromList :: Ord a => [a] -> RBTree a
fromList = foldl (flip insert) Empty
