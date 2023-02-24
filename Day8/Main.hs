module Day8.Main (main) where

import Data.List (transpose)
import Control.Monad (guard)

import My.Util (count)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (b : bs)
    | f b = [b]
    | otherwise = b : (takeUntil f bs)

mapWithLinesOfSight :: (Int -> [[Int]] -> a) -> [[Int]] -> [[a]]
mapWithLinesOfSight f trees = do
    (i, row) <- zip [0 ..] trees
    return $ do
        (j, size) <- zip [0 ..] row
        let col = transpose trees !! j
        return $ f size [
            reverse $ take j row,
            drop (j + 1) row,
            reverse $ take i col,
            drop (i + 1) col]

visibilities :: [[Int]] -> [[Bool]]
visibilities = mapWithLinesOfSight $ \size linesOfSight ->
    any (all (< size)) linesOfSight

scenicScores :: [[Int]] -> [[Int]]
scenicScores =  mapWithLinesOfSight $ \size linesOfSight ->
    product $ map (length . takeUntil (>= size)) linesOfSight

main :: IO (String, String)
main = do
    trees <- map (map (read . return)) . lines <$> readFile "Day8/input.txt"

    let visibleTrees = count (== True) $ concat $ visibilities trees
    putStr "Visible trees: "
    print visibleTrees

    let maxScenicScore = maximum $ concat $ scenicScores trees
    putStr "Maximum scenic score: "
    print maxScenicScore

    return (show visibleTrees, show maxScenicScore)