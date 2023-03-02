module Day15.Main (main) where

import Data.List
import Data.Maybe
import Control.Monad

import My.Parser

slice :: Int -> (Int, Int, Int, Int) -> Maybe (Int, Int)
slice line (sx, sy, bx, by) =
    let distB = abs (sx - bx) + abs (sy - by)
        distL = abs (line - sy)
        dist = distB - distL
    in do
        guard $ dist >= 0
        return (sx - dist, sx + dist)

reduce :: [(Int, Int)] -> [(Int, Int)]
reduce [] = []
reduce [s] = [s]
reduce ((s1, e1) : (s2, e2) : rest) =
    if s2 <= e1 then
        reduce $ (s1, max e1 e2) : rest
    else
        (s1, e1) : reduce ((s2, e2) : rest)

count :: [(Int, Int)] -> Int
count = sum . map (\(s, e) -> e - s)

parseSB :: Parser (Int, Int, Int, Int)
parseSB = parserRegex "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)" (\[sx, sy, bx, by] -> (read sx, read sy, read bx, read by))

parse :: String -> (Int, Int, Int, Int)
parse = fromJust . run parseSB

main :: IO (String, String)
main = do
    sbs <- map parse . lines <$> readFile "Day15/input.txt"
    let line = sbs >>= maybeToList . slice 2000000
        result1 = count $ reduce $ sort line
    putStrLn "Scanned positions in row 2000000:"
    putStrLn $ show result1
    let (bx, by) = head $ do
            y <- [0 .. 4000000]
            let lineY = sbs >>= maybeToList . slice y
                r = reduce $ sort lineY
            guard $ length r == 2
            return $ ((+ 1) $ snd $ head r, y)
    putStrLn "Position of missing beacon:"
    putStrLn $ show $ 4000000 * bx + by
    return (show result1, show $ 4000000 * bx + by)