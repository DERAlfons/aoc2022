module Day09.Main (main) where

import Data.List (scanl', nub)

type Point = (Int, Int)

step :: Char -> Point -> Point
step 'U' (x, y) = (x, y + 1)
step 'D' (x, y) = (x, y - 1)
step 'L' (x, y) = (x - 1, y)
step 'R' (x, y) = (x + 1, y)

follow :: Point -> Point -> Point
follow (xH, yH) (xT, yT) = case (abs (xH - xT), abs (yH - yT)) of
    (2, 2) -> ((xH + xT) `div` 2, (yH + yT) `div` 2)
    (2, _) -> ((xH + xT) `div` 2, yH)
    (_, 2) -> (xH, (yH + yT) `div` 2)
    (_, _) -> (xT, yT)

update :: [Point] -> Char -> [Point]
update (pH : pTs) direction = scanl' follow (step direction pH) pTs

parseInstruction :: String -> [Char]
parseInstruction s =
    let [[direction], count] = words s in
    replicate (read count) direction

main :: IO (String, String)
main = do
    instructions <- (parseInstruction =<<) . lines <$> readFile "Day09/input.txt"

    let ropePositions2 = scanl' update (replicate 2 (0, 0)) instructions
        lastKnotPosCount2 = length $ nub $ map last ropePositions2
    putStr "Number of positions visited by end of rope of length 2: "
    print lastKnotPosCount2

    let ropePositions10 = scanl' update (replicate 10 (0, 0)) instructions
        lastKnotPosCount10 = length $ nub $ map last ropePositions10
    putStr "Number of positions visited by end of rope of length 10: "
    print lastKnotPosCount10

    return (show lastKnotPosCount2, show lastKnotPosCount10)