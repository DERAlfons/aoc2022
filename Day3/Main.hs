module Day3.Main (main) where

import Data.List
import Data.Char

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n bs =
    let (g, rest) = splitAt n bs
    in g : groupsOf n rest

prio :: Char -> Int
prio c
    | ord c <= 90 = ord c - 64 + 26
    | otherwise = ord c - 96

main :: IO (String, String)
main = do
    rucksacks <- lines <$> readFile "Day3/input.txt"
    let compartments = map (\x -> splitAt ((length x) `div` 2) x) rucksacks
        wrongItems = map (\(x, y) -> head $ intersect x y) compartments
        prioSum1 = sum $ map prio wrongItems
    putStrLn "Sum of priorities of wrongly inserted items:"
    putStrLn $ show prioSum1
    let groups = groupsOf 3 rucksacks
        idBadges = map (head . foldl1' intersect) groups
        prioSum2 = sum $ map prio idBadges
    putStrLn "Sum of priorities of ID badges:"
    putStrLn $ show prioSum2
    return (show prioSum1, show prioSum2)