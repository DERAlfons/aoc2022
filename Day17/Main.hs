module Day17.Main (main) where

import Data.List (elemIndex)
import Data.Set (Set, fromList, union, notMember, lookupMax)
import qualified Data.Set as S
import Data.Maybe (fromJust)

type State = (Int, Int, [Int])

hBar :: Int -> [(Int, Int)]
hBar h = [(2, h + 4), (3, h + 4), (4, h + 4), (5, h + 4)]

plus :: Int -> [(Int, Int)]
plus h = [(3, h + 6), (2, h + 5), (3, h + 5), (4, h + 5), (3, h + 4)]

angle :: Int -> [(Int, Int)]
angle h = [(4, h + 6), (4, h + 5), (2, h + 4), (3, h + 4), (4, h + 4)]

vBar :: Int -> [(Int, Int)]
vBar h = [(2, h + 7), (2, h + 6), (2, h + 5), (2, h + 4)]

block :: Int -> [(Int, Int)]
block h = [(2, h + 5), (3, h + 5), (2, h + 4), (3, h + 4)]

rocks :: [Int -> [(Int, Int)]]
rocks = cycle [hBar, plus, angle, vBar, block]

fall :: Int -> Set (Int, Int) -> [(Int, Int)] -> String -> [Int -> [(Int, Int)]] -> Int
fall height pillar rock (d : ds) rocks =
    let windRock = move d rock
        realWindRock = if all ((>= 0) . fst) windRock && all ((< 7) . fst) windRock && all (flip notMember pillar) windRock then windRock else rock
        fallRock = map (\(x, y) -> (x, y - 1)) realWindRock
    in if all (flip notMember pillar) fallRock then fall height pillar fallRock ds rocks else
        let newHeight = max (snd $ head realWindRock) height
        in case rocks of
            [] -> newHeight
            (nextRock : rest) -> fall newHeight (union pillar $ fromList realWindRock) (nextRock newHeight) ds rest

fall2 :: [State] -> [(Int, Int)] -> Int -> Int -> Int -> Set (Int, Int) -> [(Int, Int)] -> [String] -> [Int -> [(Int, Int)]] -> [(Int, Int)]
fall2 s (h : hs) rId rDrop height pillar rock ([] : dss) rocks =
    let structure = getStructure height pillar
        state = (rId, rDrop, structure)
    in if elem state s then (h : hs) else fall2 (state : s) (h : h : hs) rId rDrop height pillar rock dss rocks
fall2 s ((hInfo, bCount) : hs) rId rDrop height pillar rock ((d : ds) : dss) (nextRock : rocks) =
    let windRock = move d rock
        realWindRock = if all ((>= 0) . fst) windRock && all ((< 7) . fst) windRock && all (flip notMember pillar) windRock then windRock else rock
        fallRock = map (\(x, y) -> (x, y - 1)) realWindRock
    in if all (flip notMember pillar) fallRock then fall2 s ((hInfo, bCount) : hs) rId (rDrop + 1) height pillar fallRock (ds : dss) (nextRock : rocks) else
        let newHeight = max (snd $ head realWindRock) height
        in fall2 s ((newHeight, bCount + 1) : hs) ((rId + 1) `mod` 5) 0 newHeight (union pillar $ fromList realWindRock) (nextRock newHeight) (ds : dss) rocks

getStructure :: Int -> Set (Int, Int) -> [Int]
getStructure height pillar = map ((subtract height) . snd . fromJust . lookupMax) [
    S.filter ((== 0) . fst) pillar,
    S.filter ((== 1) . fst) pillar,
    S.filter ((== 2) . fst) pillar,
    S.filter ((== 3) . fst) pillar,
    S.filter ((== 4) . fst) pillar,
    S.filter ((== 5) . fst) pillar,
    S.filter ((== 6) . fst) pillar]

move :: Char -> [(Int, Int)] -> [(Int, Int)]
move '>' = map (\(x, y) -> (x + 1, y))
move '<' = map (\(x, y) -> (x - 1, y))

main :: IO (String, String)
main = do
    [wind] <- lines <$> readFile "Day17/input.txt"
    let height = fall 0 (fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0)]) (head rocks $ 0) (cycle wind) (tail $ take 2022 rocks)
    putStrLn "Height of rock tower after 2022 rocks:"
    putStrLn $ show height
    let [(heightEnd, rocksEnd), (heightStart, rocksStart)] = fall2 [] [(0, 0)] 0 0 0 (fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0)]) (head rocks $ 0) (repeat wind) (tail rocks)
        rocksStep = rocksEnd - rocksStart
        heightStep = heightEnd - heightStart
        (steps, rocksRest) = (1000000000000 - rocksStart) `divMod` rocksStep
        height2 = fall 0 (fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0)]) (head rocks $ 0) (cycle wind) (tail $ take (rocksStart + rocksRest) rocks)
        result2 = height2 + steps * heightStep
    putStrLn "Height of rock tower after 1000000000000 rocks:"
    putStrLn $ show result2
    return (show height, show result2)