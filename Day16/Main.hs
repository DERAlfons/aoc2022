module Day16.Main (main) where

import Data.List (sortOn, (\\))
import Data.Map (Map, fromList, (!))
import Data.Maybe (maybeToList)
import Control.Monad (guard)

import Algorithm.Search (dijkstra)

import My.Util (sortDesc)
import My.Parser (parserRegex, parserList, run)

type Valve = (String, Int)
data State = State {
    guys :: [(Valve, Int)],
    remainValves :: [Valve],
    eMin :: Int,
    eMax :: Int}
    deriving (Eq, Ord)

maxGain :: [(Valve, Int)] -> [Valve] -> Int
maxGain guys valves =
    let rTimes = sortDesc $ (\t -> [t - 3, t - 6 .. 0]) =<< map snd guys in
    sum $ zipWith (*) rTimes (map snd valves)

next :: Map (String, String) Int -> State -> [State]
next edges s =
    neighbours ++ stop
    where
    neighbours = do
        let (pos, remainTime) : gs = guys s
        newPos <- remainValves s
        let newRemainValves = remainValves s \\ [newPos]
            dist = edges ! (fst pos, fst newPos)
            newRemainTime = remainTime - dist - 1
        guard $ newRemainTime > 0
        let newGuys = (newPos, newRemainTime) : gs
            gain = newRemainTime * (snd newPos)
            newMin = eMin s + gain
            newMax = newMin + maxGain newGuys newRemainValves
        return $ State newGuys newRemainValves newMin newMax
    stop =
        let gs = tail $ guys s
            newMax = eMin s + maxGain gs (remainValves s) in
        [s {guys = gs, eMax = newMax}]

tCost :: State -> State -> Int
tCost s1 s2 = eMax s1 - eMax s2

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

    let me = (("AA", 0), 30)
        tMax = maxGain [me] valves
        start = State [me] valves 0 tMax
        Just (cost, _) = dijkstra (next edges) tCost (null . guys) start
        maxP = tMax - cost
    putStr "Maximal released pressure alone: "
    print $ maxP

    let me2 = (("AA", 0), 26)
        elephant = (("AA", 0), 26)
        tMax2 = maxGain [me2, elephant] valves
        start2 = State [me2, elephant] valves 0 tMax2
        Just (cost2, _) = dijkstra (next edges) tCost (null . guys) start2
        maxP2 = tMax2 - cost2
    putStr "Maximal released pressure together with elephant: "
    print maxP2

    return (show maxP, show maxP2)