module Day14.Main (main) where

import Data.Maybe (maybeToList)
import Data.Array (Array, listArray, (//))
import Data.Array.ST (STArray, thaw, readArray, writeArray)
import Control.Monad.ST (ST, runST)

import My.Parser (parserRegex, parserList, run)

type Point = (Int, Int)

countSand :: Array Point Bool -> Point -> (Point -> Bool) -> Int
countSand walls start check = runST $ loop 0 start =<< thaw walls
    where
    loop :: Int -> Point -> STArray s Point Bool -> ST s Int
    loop count (x, y) walls = do
        below <- readArray walls (x    , y + 1)
        left  <- readArray walls (x - 1, y + 1)
        right <- readArray walls (x + 1, y + 1)
        head $ concat [
            [loop count (x    , y + 1) walls | not below],
            [loop count (x - 1, y + 1) walls | not  left],
            [loop count (x + 1, y + 1) walls | not right],
            [return $ count + 1 | check (x, y)],
            [writeArray walls (x, y) True >> loop (count + 1) start walls]]

getLines :: [Point] -> [Point]
getLines points = do
    ((sx, sy), (ex, ey)) <- zip points (tail points)
    x <- [min sx ex .. max sx ex]
    y <- [min sy ey .. max sy ey]
    return (x, y)

parseRocks :: String -> Maybe [Point]
parseRocks = run $ getLines <$>
    parserList "" " -> " "$" (
        parserRegex "(\\d+),(\\d+)" $ \[s, e] -> (read s, read e))

main :: IO (String, String)
main = do
    rocks <- concat . (maybeToList . parseRocks =<<) <$>
        lines <$> readFile "Day14/input.txt"
    let maxY = (maximum $ map snd rocks) + 2
        minX = (minimum $ map fst rocks) - maxY
        maxX = (maximum $ map fst rocks) + maxY
        floor = [(x, maxY) | x <- [minX .. maxX]]
        walls = rocks ++ floor
        wallsArr = listArray ((minX, 0), (maxX, maxY)) (repeat False) //
            zip walls (repeat True)

    let sandCount1 = countSand wallsArr (500, 0) ((== maxY - 1) . snd)
    putStr "Sand units before sand spills over: "
    print $ sandCount1 - 1

    let sandCount2 = countSand wallsArr (500, 0) (== (500, 0))
    putStr "Sand units before sand reaches top: "
    print sandCount2

    return (show $ sandCount1 - 1, show sandCount2)