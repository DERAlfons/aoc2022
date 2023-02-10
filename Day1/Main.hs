module Day1.Main (
    main,
    answer1,
    answer2)
where

import Data.List (sort)
import My.Util (explode)

getCalories :: String -> [Int]
getCalories = map (sum . map read) . explode [] . lines

answer1 :: String -> String
answer1 input =
    let calories = getCalories input
        maxCals = maximum calories in
    show maxCals

answer2 :: String -> String
answer2 input =
    let calories = getCalories input
        max3Cals = sum $ take 3 $ reverse $ sort calories in
    show max3Cals

main :: IO (String, String)
main = do
    input <- readFile "Day1/input.txt"
    putStr "Maximum calories: "
    putStrLn $ answer1 input
    putStr "Calories of top 3 elves: "
    putStrLn $ answer2 input
    return (answer1 input, answer2 input)