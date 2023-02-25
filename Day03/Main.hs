module Day03.Main (main) where

import Data.List (intersect, foldl1')
import Data.Char (ord)

import My.Util (groupsOf)

prio :: Char -> Int
prio c
    | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
    | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27

main :: IO (String, String)
main = do
    rucksacks <- lines <$> readFile "Day03/input.txt"

    let compartments = map (\x -> splitAt ((length x) `div` 2) x) rucksacks
        wrongItems = map (head . uncurry intersect) compartments
        prioSum1 = sum $ map prio wrongItems
    putStr "Sum of priorities of wrongly inserted items: "
    print prioSum1

    let groups = groupsOf 3 rucksacks
        idBadges = map (head . foldl1' intersect) groups
        prioSum2 = sum $ map prio idBadges
    putStr "Sum of priorities of ID badges: "
    print prioSum2

    return (show prioSum1, show prioSum2)