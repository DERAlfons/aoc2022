module Day15.Main (main) where

import Data.List (sort)
import Data.Maybe (maybeToList, listToMaybe)
import Control.Monad (guard)

import My.Util (maybeToIO)
import My.Parser (parserRegex, run)

type Point = (Int, Int)
type Sensor = (Point, Int)
type Range = (Int, Int)

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

hSlice :: Int -> Sensor -> Maybe Range
hSlice y ((sx, sy), radius) = do
    let vDist = abs (y - sy)
        hDist = radius - vDist
    guard $ hDist >= 0
    return (sx - hDist, sx + hDist)

reduce :: [Range] -> [Range]
reduce [] = []
reduce [s] = [s]
reduce ((s1, e1) : (s2, e2) : rest)
    | s2 <= e1 = reduce $ (s1, max e1 e2) : rest
    | otherwise = (s1, e1) : reduce ((s2, e2) : rest)

count :: [Range] -> Int
count = sum . map (\(s, e) -> e - s) . reduce . sort

unscanned :: [Sensor] -> Range -> Range -> Maybe Point
unscanned sensors (minX, maxX) (minY, maxY) = listToMaybe $ do
    y <- [minY .. maxY]
    let yLine = maybeToList . hSlice y =<< sensors
        r = reduce $ sort yLine
        k = dropWhile ((< minX) . snd) r
    case k of
        [] -> return (minX, y)
        (s, e) : _
            | s > minX -> return (minX, y)
            | e < maxX -> return (e + 1, y)
        otherwise -> []

parseSensor :: String -> Maybe Sensor
parseSensor = run $ parserRegex
    "Sensor at x=(-?\\d+), y=(-?\\d+): \
        \closest beacon is at x=(-?\\d+), y=(-?\\d+)" $
    \[sx, sy, bx, by] ->
        let sensorOrigin = (read sx, read sy)
            beacon = (read bx, read by) in
        (sensorOrigin, distance sensorOrigin beacon)

main :: IO (String, String)
main = do
    sensors <- (maybeToList . parseSensor =<<) <$>
        lines <$> readFile "Day15/input.txt"

    let scanLine = maybeToList . hSlice 2000000 =<< sensors
        scanCount = count scanLine
    putStr "Scanned positions in row 2000000: "
    print scanCount

    (bx, by) <- maybeToIO "error: all positions are scanned" $
        unscanned sensors (0, 4000000) (0, 4000000)
    let tuningFrequency = 4000000 * bx + by
    putStr "Position of missing beacon: "
    print tuningFrequency

    return (show scanCount, show tuningFrequency)