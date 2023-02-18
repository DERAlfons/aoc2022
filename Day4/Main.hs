module Day4.Main (main) where

import Text.Regex.PCRE ((=~), mrSubList)

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count a (b : bs)
    | a == b = 1 + count a bs
    | a /= b = count a bs

contains :: ((Int, Int), (Int, Int)) -> Bool
contains ((s1, e1), (s2, e2)) = (s1 <= s2 && e1 >= e2) || (s1 >= s2 && e1 <= e2)

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((s1, e1), (s2, e2)) = (e1 >= s2) && (s1 <= e2)

parse :: String -> ((Int, Int), (Int, Int))
parse str =
    let [s1, e1, s2, e2] = map read $ mrSubList $ str =~ "^([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)$"
    in ((s1, e1), (s2, e2))

main :: IO (String, String)
main = do
    areaPairs <- map parse <$> lines  <$> readFile "Day4/input.txt"
    let fullyContainedCount = count True $ map contains areaPairs
    putStrLn "Number of pairs with fully contained area:"
    putStrLn $ show fullyContainedCount
    let overlapCount = count True $ map overlap areaPairs
    putStrLn "Number of pairs with overlap:"
    putStrLn $ show overlapCount
    return (show fullyContainedCount, show overlapCount)