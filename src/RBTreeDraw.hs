module RBTreeDraw where

import           RBTreeInternal                as RB
import           Graphics.Gloss

-- Data type grouping gloss settings and rendering settings.
-- Function calls started geting complicated so I used constants for the 
-- rendering stuff.
data Settings = Settings {
    window                :: Display,
    background            :: Color
--   circleRadius         :: Float,
--   verticalFactor       :: Float,
--   horizontalFactor     :: Float,
--   textXDisplacement    :: Float,
--   textYDisplacement    :: Float 
    }

-- The drawable model of a tree that gets passed to Gloss
-- It's composed of a red black tree and a list of elements yet to be inserted
data Model a = Model (RBTree a) [a]

defaultSettings :: Settings
defaultSettings = Settings
    { window               = InWindow "Okasaki Red-Black Trees" (800, 600) (0, 0)
    , background           = white
    -- , circleRadius      = 50
    -- , verticalFactor    = 2.5
    -- , horizontalFactor  = 2
    -- , textXDisplacement = -10
    -- , textYDisplacement = -12.5
    }

circleRadius :: Float
circleRadius = 25

verticalFactor :: Float
verticalFactor = 2.5

horizontalFactor :: Float
horizontalFactor = 2

textXDisplacement :: Float
textXDisplacement = -10

textYDisplacement :: Float
textYDisplacement = -12.5

-- Calculate how far to the left or right a node needs to be placed, 
-- in relation to its parent.
displacementX :: RBTree a -> Float
displacementX Empty = circleRadius * ((w + 1) / 2.0) where w = width Empty
displacementX tree  = circleRadius * ((w + 1) / 2.0)
  where
    w = width l + width r
    l = left tree
    r = right tree

-- How much horizontal space a node needs to reserve
width :: RBTree a -> Float
width Empty                    = horizontalFactor
width (Branch _ _ Empty Empty) = horizontalFactor * 1.6
width (Branch _ _ left  Empty) = horizontalFactor + width left
width (Branch _ _ Empty right) = horizontalFactor + width right
width (Branch _ _ left  right) = width right + width right

-- Vertical spacing between nodes
displacementY :: Float
displacementY = verticalFactor * circleRadius

-- Recursively draw a tree starting at (x, y) coordinates
drawTree :: Show a => RBTree a -> Float -> Float -> Picture
drawTree Empty x y = Color black $ Translate x y $ circleSolid circleRadius
drawTree (Branch c n l r) x y =
    let displacement = displacementX (Branch c n l r)
    in  pictures
            [ drawNode n x y c
            , drawTree l (x - displacement) (y - displacementY)
            , line
                [ (x               , y - circleRadius)
                , (x - displacement, y - displacementY + circleRadius)
                ]
            , drawTree r (x + displacement) (y - displacementY)
            , line
                [ (x               , y - circleRadius)
                , (x + displacement, y - displacementY + circleRadius)
                ]
            ]

drawNode :: Show a => a -> Float -> Float -> RBColor -> Picture
drawNode n x y c = pictures
    [ drawNodeCircle x y c
    , drawNodeText n (x + textXDisplacement) (y + textYDisplacement)
    ]

drawLine :: Float -> Float -> Float -> Picture
drawLine x y displacement = line
    [ (x               , y - circleRadius)
    , (x - displacement, y - displacementY + circleRadius)
    ]

drawNodeText :: Show a => a -> Float -> Float -> Picture
drawNodeText n x y =
    let string = show n
        scale  = 1.0 / (fromIntegral (length string) * 4.0)
    in  Color white $ Translate x y $ Scale scale scale $ text string

drawNodeCircle :: Float -> Float -> RBColor -> Picture
drawNodeCircle x y c = Color (toColor c) $ Translate x y $ circleSolid circleRadius
  where
    toColor :: RBColor -> Color
    toColor Red   = red
    toColor Black = black

-- Draw a tree
draw :: Show a => RBTree a -> Settings -> IO ()
draw tree settings = display w bg $ drawTree tree 0 0
  where
    w  = window settings
    bg = background settings

-- Draw a tree building animation
incrementalDraw :: (Show a, Ord a) => [a] -> Settings -> IO ()
incrementalDraw []       settings = error "Can't build tree from no input"
incrementalDraw (x : xs) settings = simulate w bg 1 initial drawModel updateModel
  where
    w                        = window settings
    bg                       = background settings
    initial                  = Model (singleton x) xs
    drawModel (Model tree _) = drawTree tree 0 0
    updateModel _ _          = update

update :: Ord a => Model a -> Model a
update (Model tree []      ) = Model tree []
update (Model tree (x : xs)) = Model tree' xs where tree' = insert x tree
