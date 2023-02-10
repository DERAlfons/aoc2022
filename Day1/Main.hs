module Day1.Main (
    main,
    answer1,
    answer2)
where

import Data.List (sort)

explode :: Eq a => a -> [a] -> [[a]]
explode _ [] = []
explode sep lst =
    let elm = takeWhile (/= sep) lst
        rst = dropWhile (/= sep) lst
    in case rst of
        [] -> [elm]
        (_ : rst2) -> elm : (explode sep rst2)

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

main :: IO ()
main = do
    input <- readFile "Day1/input.txt"
    putStrLn "Maximum calories:"
    putStrLn $ answer1 input
    putStrLn "Calories of top 3 elves:"
    putStrLn $ answer2 input
    return ()