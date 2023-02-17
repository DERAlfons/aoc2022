module Day2.Main (main) where

data RPS = Rock | Paper | Scissors deriving Eq

parse :: String -> RPS
parse "A" = Rock
parse "B" = Paper
parse "C" = Scissors
parse "X" = Rock
parse "Y" = Paper
parse "Z" = Scissors

rpsPoints :: RPS -> Int
rpsPoints Rock = 1
rpsPoints Paper = 2
rpsPoints Scissors = 3

matchPoints :: RPS -> RPS -> Int
matchPoints Rock Paper = 6
matchPoints Paper Scissors = 6
matchPoints Scissors Rock = 6
matchPoints a b
    | a == b = 3
matchPoints _ _ = 0

win :: RPS -> RPS
win Rock = Paper
win Paper = Scissors
win scissors = Rock

draw :: RPS -> RPS
draw = id

lose :: RPS -> RPS
lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

points1 :: [String] -> Int
points1 [a, b] = rpsPoints (parse b) + matchPoints (parse a) (parse b)

points2 :: [String] -> Int
points2 [a, "X"] = 0 + (rpsPoints $ lose $ parse a)
points2 [a, "Y"] = 3 + (rpsPoints $ draw $ parse a)
points2 [a, "Z"] = 6 + (rpsPoints $ win $ parse a)

main :: IO (String, String)
main = do
    strat <- map words <$> lines <$> readFile "Day2/input.txt"
    let score1 = sum $ map points1 strat
    putStr "Total score with code 1: "
    print score1
    let score2 = sum $ map points2 strat
    putStr "Total score with code 2: "
    print score2
    return (show score1, show score2)