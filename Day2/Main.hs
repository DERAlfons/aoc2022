module Day2.Main (main) where

import Data.Char (ord)

data RPS = Rock | Paper | Scissors deriving (Eq, Enum)

toRPS :: Char -> RPS
toRPS c = toEnum $ (ord c - ord 'A') `mod` (ord 'X' - ord 'A')

toAction :: Char -> (RPS -> RPS)
toAction 'X' = lose
toAction 'Y' = draw
toAction 'Z' = win

parseMatch1 :: [Char] -> (RPS, RPS)
parseMatch1 [c1, c2] = (toRPS c1, toRPS c2)

parseMatch2 :: [Char] -> (RPS, RPS -> RPS)
parseMatch2 [c1, c2] = (toRPS c1, toAction c2)

rpsPoints :: RPS -> Int
rpsPoints rps = fromEnum rps + 1

matchPoints :: RPS -> RPS -> Int
matchPoints rps1 rps2
    | rps1 == win  rps2 = 6
    | rps1 == draw rps2 = 3
    | rps1 == lose rps2 = 0

points :: RPS -> RPS -> Int
points rps1 rps2 = rpsPoints rps1 + matchPoints rps1 rps2

win :: RPS -> RPS
win rps = toEnum $ (fromEnum rps + 1) `mod` 3

draw :: RPS -> RPS
draw = id

lose :: RPS -> RPS
lose rps = toEnum $ (fromEnum rps - 1) `mod` 3

main :: IO (String, String)
main = do
    instructions <- map (map head . words) . lines <$> readFile "Day2/input.txt"

    let strategy1 = map parseMatch1 instructions
        score1 = sum $ map (\(rps1, rps2) -> points rps2 rps1) strategy1
    putStr "Total score with code 1: "
    print score1

    let strategy2 = map parseMatch2 instructions
        score2 = sum $ map (\(rps, act) -> points (act rps) rps) strategy2
    putStr "Total score with code 2: "
    print score2

    return (show score1, show score2)