module Day25.Main (main) where

import Data.List (foldl1')

value :: Char -> Int
value '=' = -2
value '-' = -1
value '0' =  0
value '1' =  1
value '2' =  2

symbol :: Int -> Char
symbol (-2) = '='
symbol (-1) = '-'
symbol   0  = '0'
symbol   1  = '1'
symbol   2  = '2'

addSnafu :: [Int] -> [Int] -> [Int]
addSnafu a b = reverse $ loop (reverse a) (reverse b) 0
    where
    loop [] [] 0 = []
    loop [] [] c = [c]
    loop a [] c = loop a [0] c
    loop [] b c = loop [0] b c
    loop (da : a) (db : b) c = let
        (carry, d) = (da + db + c + 2) `divMod` 5 in
        (d - 2) : loop a b carry

main :: IO (String, String)
main = do
    fuelList <- map (map value) . lines <$> readFile "Day25/input.txt"

    let totalFuel = map symbol $ foldl1' addSnafu fuelList
    putStr "Total fuel: "
    putStrLn totalFuel

    return (totalFuel, "No second part :)")