module Day01.Main (main) where

import My.Util (explode, sortDesc)

main :: IO (String, String)
main = do
    calories <- map (sum . map read) . explode [] . lines <$>
        readFile "Day01/input.txt" :: IO [Int]
    let maxCals = maximum calories
        max3Cals = sum $ take 3 $ sortDesc calories
    putStr "Maximum calories: "
    print maxCals
    putStr "Calories of top 3 elves: "
    print max3Cals
    return (show maxCals, show max3Cals)