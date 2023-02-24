module Day9.Main (main) where

import Data.List

import My.Util (explode)

step :: Char -> (Int, Int) -> (Int, Int)
step 'U' (x, y) = (x, y + 1)
step 'D' (x, y) = (x, y - 1)
step 'L' (x, y) = (x - 1, y)
step 'R' (x, y) = (x + 1, y)

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (xH, yH) (xT, yT) = case (abs (xH - xT), abs (yH - yT)) of
    (2, 2) -> ((xH + xT) `div` 2, (yH + yT) `div` 2)
    (2, _) -> ((xH + xT) `div` 2, yH)
    (_, 2) -> (xH, (yH + yT) `div` 2)
    (_, _) -> (xT, yT)

update :: [(Int, Int)] -> Char -> [(Int, Int)]
update (pH : pTs) direction = scanl' follow (step direction pH) pTs

parseInstruction :: String -> [Char]
parseInstruction s =
    let [[direction], count] = explode ' ' s in
    replicate (read count) direction

main :: IO (String, String)
main = do
    instructions <- concat . map parseInstruction . lines <$> readFile "Day9/input.txt"
    let htPositions2 = scanl' update [(0, 0), (0, 0)] instructions
        tPosCount2 = length $ nub $ map last htPositions2
    putStrLn "Number of positions visited by end of rope of length 2:"
    putStrLn $ show tPosCount2
    let htPositions10 = scanl' update (replicate 10 (0, 0)) instructions
        tPosCount10 = length $ nub $ map last htPositions10
    putStrLn "Number of positions visited by end of rope of length 10:"
    putStrLn $ show tPosCount10
    return (show tPosCount2, show tPosCount10)