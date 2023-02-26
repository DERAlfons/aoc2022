module Day12.Main (main) where

import Data.List
import Data.Map (Map, fromList, (!))
import Data.Char (ord)
import Control.Monad

import Algorithm.Search (dijkstraAssoc)

graph :: (Int -> Int -> Bool) -> [[Int]] -> Map (Int, Int) [((Int, Int), Int)]
graph condition hMap = fromList $ do
    (i, row) <- zip [0 ..] hMap
    (j, h) <- zip [0 ..] row
    return ((i, j), do
        (iN, jN) <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
        guard $ (iN >= 0) && (jN >= 0) && (iN < length hMap) && (jN < length row)
        guard $ condition h (hMap !! iN !! jN)
        return ((iN, jN), 1))

main :: IO (String, String)
main = do
    seMap <- lines <$> readFile "Day12/input.txt"
    let Just jS = msum $ map (elemIndex 'S') seMap
        Just iS = msum $ map (elemIndex 'S') $ transpose seMap
        Just jE = msum $ map (elemIndex 'E') seMap
        Just iE = msum $ map (elemIndex 'E') $ transpose seMap
        hMap = [[case e of {'S' -> ord 'a'; 'E' -> ord 'z'; x -> ord x} | e <- row] | row <- seMap]
        graphM = graph (\h n -> n <= h + 1) hMap
        Just (cost, _) = dijkstraAssoc (graphM !) (== (iE, jE)) (iS, jS)
    putStrLn "Shortest path from start to top:"
    putStrLn $ show cost
    let graphMi = graph (\h n -> h <= n + 1) hMap
        Just (costI, _) = dijkstraAssoc (graphMi !) (\(i, j) -> hMap !! i !! j == ord 'a') (iE, jE)
    putStrLn "Shortest path from any lowest point to top:"
    putStrLn $ show costI
    return (show cost, show costI)