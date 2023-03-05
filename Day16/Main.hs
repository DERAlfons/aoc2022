module Day16.Main (main) where

import Data.List (sortOn, (\\))
import Data.Map (Map, fromList, (!))
import Data.Maybe (maybeToList)
import Control.Monad (guard)

import Algorithm.Search (dijkstra, dijkstraAssoc)

import My.Parser (parserRegex, parserList, run)

type Valve = (String, Int)
type State = ([Valve], [Valve], Int, Int, [Int])

maxGain :: [Int] -> [Valve] -> Int
maxGain _ [] = 0
maxGain [] _ = 0
maxGain [t] vs = maxGain [t, 0] vs
maxGain [t1, t2] vs @ ((_, v) : rest)
    | t1 < t2 = maxGain [t2, t1] vs
    | t1 <= 3 = 0
    | otherwise = (t1 - 3) * v + maxGain [t1 - 3, t2] rest

next :: Map (String, String) Int -> State -> [(State, Int)]
next edges (pos : ps, remainValves, eMin, eMax, remainTime : rts) =
    neighbours ++ stop
    where
    neighbours = do
        newPos <- remainValves
        let newRemainValves = remainValves \\ [newPos]
            dist = edges ! (fst pos, fst newPos)
            newRemainTime = remainTime - dist - 1
        guard $ newRemainTime > 0
        let gain = newRemainTime * (snd newPos)
            newMin = eMin + gain
            newMax = newMin + maxGain (newRemainTime : rts) newRemainValves
        return ((newPos : ps, newRemainValves, newMin, newMax, newRemainTime : rts), eMax - newMax)

    stop =
        let newMax = eMin + maxGain rts remainValves in
        [((ps, remainValves, eMin, newMax, rts), eMax - newMax)]

parseValve :: String -> Maybe (Valve, [String])
parseValve = run $ (,) <$>
    parserRegex "Valve ([A-Z]+) has flow rate=(\\d+); " (
        \[s, n] -> (s, read n)) <*>
    parserList "tunnels? leads? to valves? " ", " "$" (
        parserRegex "([A-Z]+)" (\[s] -> s))

main :: IO (String, String)
main = do
    (nodes, nNodes) <- unzip . (maybeToList . parseValve =<<) <$>
        lines <$> readFile "Day16/input.txt"
    let valves = reverse $ sortOn snd $ filter ((/= 0) . snd) nodes
        graph = fromList $ zip (map fst nodes) nNodes
        edges = fromList $ do
            start <- "AA" : map fst valves
            end <- map fst valves
            let Just (dist, _) = dijkstra (graph !) (\_ _ -> 1) (== end) start
            return ((start, end), dist)

    let tMax = maxGain [30] valves
        start = ([("AA", 0)], valves, 0, tMax, [30])
        Just (cost, _) = dijkstraAssoc (next edges) (\(_, _, _, _, t) -> t == []) start
        maxP = tMax - cost
    putStr "Maximal released pressure alone: "
    print $ maxP

    let tMax2 = maxGain [26, 26] valves
        start2 = ([("AA", 0), ("AA", 0)], valves, 0, tMax2, [26, 26])
        Just (cost2, _) = dijkstraAssoc (next edges) (\(_, _, _, _, t) -> t == []) start2
        maxP2 = tMax2 - cost2
    putStr "Maximal released pressure together with elephant: "
    print maxP2

    return (show maxP, show maxP2)