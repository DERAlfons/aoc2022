module Day8.Main (main) where

import Data.List
import Control.Monad

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (b : bs)
    | f b = [b]
    | otherwise = b : (takeUntil f bs)

visible :: [[Int]] -> Int
visible trees = length $ do
    (i, row) <- zip [0 ..] trees
    (j, size) <- zip [0 ..] row
    let col = transpose trees !! j
    guard $ or $ map (all (< size)) [
        take j row,
        drop (j + 1) row,
        take i col,
        drop (i + 1) col]
    return ()

scenic_score :: [[Int]] -> [[Int]]
scenic_score trees = do
    (i, row) <- zip [0 ..] trees
    return $ do
        (j, size) <- zip [0 ..] row
        let col = transpose trees !! j
        return $ product $ map (length . takeUntil (>= size)) [
            reverse $ take j row,
            drop (j + 1) row,
            reverse $ take i col,
            drop (i + 1) col]

main :: IO (String, String)
main = do
    trees <- map (map (read . return)) <$> lines <$> readFile "Day8/input.txt"
    putStrLn "Visible trees:"
    putStrLn $ show $ visible trees
    putStrLn "Maximum scenic score:"
    putStrLn $ show $ maximum $ map maximum $ scenic_score trees
    return (show $ visible trees, show $ maximum $ map maximum $ scenic_score trees)