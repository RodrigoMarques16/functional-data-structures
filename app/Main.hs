module Main where

import           Graphics.Gloss
import           BSTree

-- data Settings = Settings { circleWidth :: Float }

textXDisplacement :: Float
textXDisplacement = (-10)

textYDisplacement :: Float
textYDisplacement = (-12.5)

circleWidth :: Float
circleWidth = 50

drawTree :: Show a => BSTree a -> Float -> Float -> Float -> Picture
drawTree Empty                 h x y = Translate x y $ circleSolid circleWidth
drawTree (Branch n left right) h x y = pictures
    [ drawNode n x y
    , drawTree left
               (h - 1)
               (x - circleWidth * h)
               (y - (2 * circleWidth))
    , line
        [ (x, y)
        , (x - circleWidth * h, y - (2 * circleWidth))
        ]
    , drawTree right
               (h - 1)
               (x + circleWidth * h)
               (y - (2 * circleWidth))
    , line
        [ (x, y)
        , (x + circleWidth * h, y - (2 * circleWidth))
        ]
    ]

drawNode :: Show a => a -> Float -> Float -> Picture
drawNode n x y = pictures
    [ drawNodeCircle x y
    , drawNodeText n (x + textXDisplacement) (y + textYDisplacement)
    ]

drawNodeText :: Show a => a -> Float -> Float -> Picture
drawNodeText n x y =
    let string = show n
        scale  = 1.0 / (fromIntegral (length string) * 4.0)
    in  Translate x y $ Scale scale scale $ text string

drawNodeCircle :: Float -> Float -> Picture
drawNodeCircle x y = Translate x y $ circle circleWidth

main :: IO ()
main = display window white $ drawTree tree (fromIntegral h) 0 0
    where window = InWindow "Functional BST" (800, 600) (0, 0)
          h = height tree

tree :: BSTree Int
tree = fromList [1,2,3,4,5,6]