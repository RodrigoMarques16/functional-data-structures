module Main where

import           Graphics.Gloss
import           Control.Monad
import           System.Console.Haskeline hiding ( display )
import           Control.Monad.Trans
import           Text.Read
import           RBTree

-- data Settings = Settings { circleRadius :: Float }

data Model a = Model (RBTree a) [a]

data ParseResult = Quit | Draw [String]

textXDisplacement :: Float
textXDisplacement = (-10)

textYDisplacement :: Float
textYDisplacement = (-12.5)

circleRadius :: Float
circleRadius = 50

fps :: Int
fps = 60

window :: Display
window = InWindow "Functional BST" (800, 600) (0, 0)

help :: String
help = "tough luck"

displacementOf :: RBTree a -> Float
displacementOf Empty = circleRadius * ((w + 1) / 2.0)
    where w = fromIntegral $ width Empty
displacementOf tree = circleRadius * ((w + 1) / 2.0)
    where w = fromIntegral $ (width l) + (width r)
          l = left tree
          r = right tree

displacementVertical :: Float
displacementVertical = 2.5 * circleRadius

drawTree :: Show a => RBTree a -> Float -> Float -> Picture
drawTree Empty x y = Color red $ Translate x y $ circleSolid circleRadius
drawTree tree x y =
    let displacement = displacementOf tree
        l = left tree
        r = right tree
        n = value tree
        c = RBTree.color tree
    in  pictures
            [ drawNode n x y c
            , drawTree l (x - displacement) (y - displacementVertical)
            , line
                [ (x               , y - circleRadius)
                , (x - displacement, y - displacementVertical + circleRadius)
                ]
            , drawTree r (x + displacement) (y - displacementVertical)
            , line
                [ (x               , y - circleRadius)
                , (x + displacement, y - displacementVertical + circleRadius)
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
    , (x - displacement, y - displacementVertical + circleRadius)
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

draw :: Show a => RBTree a -> IO ()
draw tree = display window white $ drawTree tree 0 0

incrementalDraw :: (Show a, Ord a) => [a] -> IO ()
incrementalDraw []       = error "Can't build tree from no input"
incrementalDraw (x : xs) = simulate window white 1 initial drawModel updateModel
  where
    initial = Model (singleton x) xs
    drawModel (Model tree _) = drawTree tree 0 0
    updateModel _ dt = update

update :: Ord a => Model a -> Model a
update (Model tree []      ) = (Model tree [])
update (Model tree (x : xs)) = Model tree' xs where tree' = insert x tree

parse :: Maybe String -> ParseResult
parse minput = case minput of
    Nothing     -> Quit
    Just "quit" -> Quit
    Just ":q"   -> Quit
    Just minput -> Draw $ words minput

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "> "
        case parse minput of
            Quit -> return ()
            Draw nums ->
                liftIO (incrementalDraw (map read nums :: [Int])) >> loop

testtree  = fromList [5,3,7,2,8]
testtree2 = fromList [5,2,8,1,3,7,9,4,10]