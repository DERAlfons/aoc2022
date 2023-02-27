module Day12.Main (main) where

import Prelude hiding (lookup, filter)
import Data.Map (Map, fromList, mapWithKey, insert, lookup, (!), filter, keys)
import Data.Maybe (maybeToList)
import Control.Monad (guard)

import Algorithm.Search (dijkstraAssoc)

import My.Util (maybeToIO)

type Point = (Int, Int)

graph :: (Char -> Char -> Bool) -> Map Point Char -> Map Point [(Point, Int)]
graph condition hMap = flip mapWithKey hMap $ \(i, j) h -> do
    pN <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
    hN <- maybeToList $ lookup pN hMap
    guard $ condition h hN
    return (pN, 1)

main :: IO (String, String)
main = do
    input <- lines <$> readFile "Day12/input.txt"
    let seMap = fromList [((i, j), h) |
            (i, row) <- zip [0 ..] input,
            (j, h) <- zip [0 ..] row]
        start = head $ keys $ filter (== 'S') seMap
        end = head $ keys $ filter (== 'E') seMap
        hMap = insert start 'a' $ insert end 'z' seMap

    let graphUp = graph (\h hN -> hN <= succ h) hMap
    (pathLength1, _) <- maybeToIO "no path found" $
        dijkstraAssoc (graphUp !) (== end) start
    putStr "Shortest path from start to top: "
    print pathLength1

    let graphDown = graph (\h hN -> h <= succ hN) hMap
    (pathLength2, _) <- maybeToIO "no path found" $
        dijkstraAssoc (graphDown !) (\p -> hMap ! p == 'a') end
    putStr "Shortest path from any lowest point to top: "
    print pathLength2

    return (show pathLength1, show pathLength2)