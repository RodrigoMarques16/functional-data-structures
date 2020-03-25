module Main where

import           RBTree                        as RB
import           System.Console.Haskeline      as HL
import           Control.Monad.Trans            ( liftIO )
import           Text.Read                      ( readMaybe )

data ParseResult = Quit | Fail | Draw [Int]

parse :: Maybe String -> ParseResult
parse minput = case minput of
    Nothing     -> Quit
    Just "quit" -> Quit
    Just ":q"   -> Quit
    Just ""     -> Fail
    Just minput -> myMap readMaybe (words minput)

-- adapted from mapMaybe in Data.Maybe
-- propagates a read failure instead of silently continuing
myMap :: (a -> Maybe Int) -> [a] -> ParseResult
myMap f [] = Draw []
myMap f (x : xs) =
    let Draw rs = myMap f xs
    in  case f x of
            Just r  -> Draw (r : rs)
            Nothing -> Fail

intro :: IO ()
intro = putStrLn "\nInsert a tree as a space seperated list of integers\n\ 
                 \Example: 1 2 3\n"

main :: IO ()
main = intro >> runInputT HL.defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "> "
        case parse minput of
            Quit -> return ()
            Fail -> loop
            Draw nums ->
                liftIO (RB.incrementalDraw nums RB.defaultSettings) >> loop
