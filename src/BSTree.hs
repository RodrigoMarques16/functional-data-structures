module BSTree where

-- First implementation, unused

data BSTree a = Empty | Branch a (BSTree a) (BSTree a) deriving (Show)

empty :: Ord a => BSTree a
empty = Empty

isEmpty :: BSTree a -> Bool
isEmpty Empty = True
isEmpty _     = False

singleton :: a -> BSTree a
singleton x = Branch x Empty Empty

pair :: Ord a => a -> (a -> BSTree a)
pair a = insert (Branch a Empty Empty)

height :: BSTree a -> Int
height Empty = 0
height (Branch _ left right) = 1 + max (height left) (height right)

insert :: Ord a => BSTree a -> a -> BSTree a
insert Empty x = Branch x Empty Empty
insert (Branch n left right) x
    | x <= n    = let left'  = insert left  x in Branch n left' right
    | otherwise = let right' = insert right x in Branch n left  right'

fromList :: Ord a => [a] -> BSTree a
fromList = foldl insert Empty
