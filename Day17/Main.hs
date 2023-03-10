module Day17.Main (main) where

import Prelude hiding (map, filter, lookup)
import qualified Data.List as L
import Data.Set (
    Set, empty, fromList,
    map, filter, findMax,
    intersection, union)
import Data.Map (Map, insert, lookup)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

type Point = (Int, Int)
type Rock = Set Point
type Tower = (Set Point, Int, Int)
type State = ([Int], Rock)

rocks :: [Rock]
rocks = cycle $ L.map fromList [
    [(2, 4), (3, 4), (4, 4), (5, 4)],
    [(3, 6), (2, 5), (3, 5), (4, 5), (3, 4)],
    [(4, 6), (4, 5), (2, 4), (3, 4), (4, 4)],
    [(2, 7), (2, 6), (2, 5), (2, 4)],
    [(2, 5), (3, 5), (2, 4), (3, 4)]]

shiftY :: Int -> Rock -> Rock
shiftY h = map (\(x, y) -> (x, y + h))

pushWind :: Char -> Set Point -> Rock -> Rock
pushWind w pile rock = fromMaybe rock $ do
    let windRock = map (\(x, y) -> (x + direction w, y)) rock
    guard $ all (\(x, _) -> 0 <= x && x <= 6) windRock
    guard $ intersection windRock pile == empty
    return windRock
    where
    direction '>' =  1
    direction '<' = -1

dropRock :: Tower -> Rock -> [Char] -> Either (Tower, [Char]) (Tower, Rock)
dropRock tower rock [] = Right (tower, rock)
dropRock tower @ (pile, height, count) rock (w : ws)
    | intersection fallRock pile == empty = dropRock tower fallRock ws
    | otherwise = Left ((newPile, newHeight, count + 1), ws)
    where
    windRock = pushWind w pile rock
    fallRock = map (\(x, y) -> (x, y - 1)) windRock
    newPile = union pile windRock
    newHeight = max height (findMax $ map snd windRock)

fall :: Tower -> [Rock] -> [Char] -> Either Tower (Tower, Rock, [Rock])
fall tower [] _ = Left tower
fall tower @ (_, height, _) (rock : rocks) wind =
    case dropRock tower (shiftY height rock) wind of
        Left (newTower, newWind) -> fall newTower rocks newWind
        Right (newTower, newRock) -> Right (newTower, newRock, rocks)

getStructure :: Tower -> [Int]
getStructure (pile, height, _) = do
    x <- [0 .. 6]
    let top = findMax $ filter ((== x) . fst) pile
    return $ snd top - height

findCycle :: Tower -> [Rock] -> [Char] -> (Int, Int, Int, Int)
findCycle tower rocks wind = loop M.empty tower rocks wind
    where
    loop :: Map State (Int, Int) -> Tower -> [Rock] -> [Char] -> (Int, Int, Int, Int)
    loop states tower rocks w = case lookup checkState states of
        Just (initHeight, initCount) ->
            (initHeight, initCount, checkHeight, checkCount)
        Nothing -> loop newStates newTower newRocks newWind
        where
        Right (checkTower, checkRock, newRocks) = fall tower rocks w
        (_, checkHeight, checkCount) = checkTower
        checkState = (getStructure checkTower, shiftY (-checkHeight) checkRock)
        newStates = insert checkState (checkHeight, checkCount) states
        Left (newTower, newWind) = dropRock checkTower checkRock wind

main :: IO (String, String)
main = do
    [wind] <- lines <$> readFile "Day17/input.txt"
    let floor = (fromList [(x, 0) | x <- [0 .. 6]], 0, 0)

    let Left (_, height1, _) = fall floor (take 2022 rocks) (cycle wind)
    putStr "Height of rock tower after 2022 rocks: "
    print height1

    let (heightStart, rocksStart, heightEnd, rocksEnd) =
            findCycle floor rocks wind
        rocksStep = rocksEnd - rocksStart
        heightStep = heightEnd - heightStart
        (steps, rocksRest) = (10 ^ 12 - rocksStart) `divMod` rocksStep
        rocksMargins = rocksStart + rocksRest
        Left (_, heightMargins, _) =
            fall floor (take rocksMargins rocks) (cycle wind)
        height2 = steps * heightStep + heightMargins
    putStr "Height of rock tower after 10 ^ 12 rocks: "
    print height2

    return (show height1, show height2)