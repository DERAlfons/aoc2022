module Day24.Main (main) where

import Data.Map (Map, fromList, fromListWith, unions, toList)
import qualified Data.Map as M
import Data.Array (Array, listArray, (//), bounds, inRange, (!))
import Control.Monad (guard)

import Algorithm.Search (dijkstraAssoc)

type Point = (Int, Int, Int)

update :: Int -> Int -> Map Point [Char] -> Map Point [Char]
update w h m = fromListWith (++) $ do
    ((x, y, z), ds) <- toList m
    d <- ds
    return $ flip (,) [d] $ case d of
        '>' -> ((x + 1) `mod` w, y, z + 1)
        '<' -> ((x - 1) `mod` w, y, z + 1)
        'v' -> (x, (y + 1) `mod` h, z + 1)
        '^' -> (x, (y - 1) `mod` h, z + 1)
        '#' -> (x, y, z + 1)

next :: Array Point Bool -> Point -> [(Point, Int)]
next m (x, y, z) = do
    let b @ (_, (_, _, maxZ)) = bounds m
    (xn, yn) <- [(x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    let pn = (xn, yn, (z + 1) `mod` (maxZ + 1))
    guard $ inRange b pn && m ! pn
    return (pn, 1)

main :: IO (String, String)
main = do
    sm <- lines <$> readFile "Day24/input.txt"
    let dimX = length (sm !! 0) - 2
        dimY = length sm - 2
        dimZ = lcm dimX dimY
        m = fromList $ do
            (i, line) <- zip [-1 ..] sm
            (j, c) <- zip [-1 ..] line
            guard $ c /= '.'
            return ((j, i, 0), [c])
        tm = take dimZ $ iterate (update dimX dimY) m
        blizzards = toList $ M.map (const False) $ unions tm
        pArr = listArray ((-1, -1, 0), (dimX, dimY, dimZ - 1)) (repeat True) //
            blizzards

    let Just (cost1, path1) = dijkstraAssoc (next pArr)
            (\(x, y, _) -> x == dimX - 1 && y == dimY)
            (0, -1, 0)
    putStr "Shortest time from start to end: "
    print cost1

    let Just (cost2, path2) = dijkstraAssoc (next pArr)
            (\(x, y, _) -> x == 0 && y == -1)
            (last path1)
        Just (cost3, _) = dijkstraAssoc (next pArr)
            (\(x, y, _) -> x == dimX - 1 && y == dimY)
            (last path2)
        totalCost = cost1 + cost2 + cost3
    putStr "Shortest time from start to end to start to end: "
    print totalCost

    return (show cost1, show totalCost)