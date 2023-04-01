module Day23.Main (main) where

import Data.Set (Set, size, notMember)
import qualified Data.Set as S
import Data.Map (Map, fromListWith)
import qualified Data.Map as M
import Control.Monad (guard)

import My.Util (applyN)

type Point = (Int, Int)

run :: (Set Point, Int) -> (Set Point, Int)
run (elves, round) = let
    (newElves, newRound) = step (elves, round) in
    if newElves == elves then
        (newElves, newRound)
    else
        run (newElves, newRound)

step :: (Set Point, Int) -> (Set Point, Int)
step (elves, round) = let
    props = fromListWith (++) $ do
        e <- S.toList elves
        return (getProp elves (round `mod` 4) e, [e])
    newElves = S.fromList $ do
        (p, es) <- M.toList props
        if length es == 1 then return p else es in
    (newElves, round + 1)

getProp :: Set Point -> Int -> Point -> Point
getProp elves rot (x, y) = let
    props = take 4 $ drop rot $ cycle [
        [(x, y - 1), (x - 1, y - 1), (x + 1, y - 1)],
        [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)],
        [(x - 1, y), (x - 1, y - 1), (x - 1, y + 1)],
        [(x + 1, y), (x + 1, y - 1), (x + 1, y + 1)]]
    cProps = map head $ filter (all (`notMember` elves)) props in
    case length cProps of
        0 -> (x, y)
        4 -> (x, y)
        otherwise -> head cProps

main :: IO (String, String)
main = do
    m <- lines <$> readFile "Day23/input.txt"
    let elves = S.fromList $ do
            (i, line) <- zip [0 ..] m
            (j, pos) <- zip [0 ..] line
            guard $ pos == '#'
            return (j, i)

    let elves10 = fst $ applyN 10 step (elves, 0)
        minX = minimum $ S.map fst elves10
        maxX = maximum $ S.map fst elves10
        minY = minimum $ S.map snd elves10
        maxY = maximum $ S.map snd elves10
        emptyGround = (maxX - minX + 1) * (maxY - minY + 1) - size elves10
    putStr "Empty ground after 10 rounds: "
    print emptyGround

    let rounds = snd $ run (elves, 0)
    putStr "Total number of rounds: "
    print rounds

    return (show emptyGround, show rounds)