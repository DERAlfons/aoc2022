module Day14.Main (main) where

import Data.List
import Data.Maybe
import Data.Array
import Data.Array.IO

import My.Parser

countSand :: IOArray (Int, Int) Bool -> (Int, Int) -> ((Int, Int) -> Bool) -> Int -> IO Int
countSand arr (sx, sy) check count = do
    below <- readArray arr (sx, sy + 1)
    if not below then
        countSand arr (sx, sy + 1) check count
    else do
        left <- readArray arr (sx - 1, sy + 1)
        if not left then
            countSand arr (sx - 1, sy + 1) check count
        else do
            right <- readArray arr (sx + 1, sy + 1)
            if not right then
                countSand arr (sx + 1, sy + 1) check count
            else if check (sx, sy) then
                return (count + 1)
            else do
                writeArray arr (sx, sy) True
                countSand arr (500, 0) check (count + 1)

getRocks :: [[Int]] -> [(Int, Int)]
getRocks line = do
    ([sx, sy], [ex, ey]) <- zip line (tail line)
    x <- [min sx ex .. max sx ex]
    y <- [min sy ey .. max sy ey]
    return (x, y)

parseRocks :: Parser [(Int, Int)]
parseRocks =
    fmap getRocks $
    parserList "" "-> " "$" $
    parserList "" "," " |$" $
    parserRegex "([0-9]+)" (\[n] -> read n)

parse :: String -> [(Int, Int)]
parse = fromJust . run parseRocks

main :: IO (String, String)
main = do
    rocks <- concat . map parse . lines <$> readFile "Day14/input.txt"
    let maxY = (maximum $ map snd rocks) + 2
        minX = (minimum $ map fst rocks) - maxY
        maxX = (maximum $ map fst rocks) + maxY
        rocksBottom = rocks ++ [(x, maxY) | x <- [minX .. maxX]]
        rocksArrI = listArray ((minX, 0), (maxX, maxY)) (repeat False) // zip rocksBottom (repeat True)
    rocksArr1 <- thaw rocksArrI
    sandCount1 <- countSand rocksArr1 (500, 0) ((== (maxY - 1)) . snd) 0
    putStrLn "Sand units before sand spills over:"
    putStrLn $ show $ sandCount1 - 1
    rocksArr2 <- thaw rocksArrI
    sandCount2 <- countSand rocksArr2 (500, 0) (== (500, 0)) 0
    putStrLn "Sand units before sand reaches top:"
    putStrLn $ show sandCount2
    return (show $ sandCount1 - 1, show sandCount2)