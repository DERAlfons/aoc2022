module Day19.Main (main) where

import Data.List (sortOn)
import Data.Maybe (maybeToList)
import Data.Map (
    Map, fromList, toList, (!),
    adjust, unionWith, mapWithKey)
import qualified Data.Map as M
import Control.Monad (guard)

import My.Util (sortDescOn)
import My.Parser (parserRegex, parserList, run)

type Blueprint = Map String (Map String Int)
type State = (Map String Int, Map String Int)

step :: Blueprint -> State -> [State]
step bp (robots, resources) = wait ++ build
    where
    wait = [(robots, unionWith (+) resources robots)]
    build = do
        (robot, needed) <- toList bp
        let consume = unionWith (-) resources needed
        guard $ all (>= 0) consume
        let gather = unionWith (+) consume robots
        return $ (adjust (+ 1) robot robots, gather)

findMax :: Int -> [State] -> Blueprint -> Int
findMax 0 states _ = maximum $ map ((! "geode") . snd) states
findMax time states bp = findMax (time - 1) newStates bp
    where
    stepStates = step bp =<< states
    rOrder r = map (r !) ["geode", "obsidian", "clay", "ore"]
    highRobots = take 100 $ sortDescOn (rOrder . fst) stepStates
    highResources = take 100 $ sortDescOn (rOrder . snd) stepStates
    reducedStates = highRobots ++ highResources
    newStates = if time `mod` 6 == 0 then reducedStates else stepStates

parseBlueprint :: String -> Maybe (Int, Blueprint)
parseBlueprint = run $ (,) <$>
    parserRegex "Blueprint (\\d+): " (\[b] -> read b) <*> (
    fromList <$> parserList "" " " "$" ((,) <$>
        parserRegex "Each (\\w+) robot costs " (\[r] -> r) <*> (
        fromList <$> parserList "" " and " "." (
            parserRegex "(\\d+) (\\w+)" $ \[n, r] -> (r, read n)))))

main :: IO (String, String)
main = do    
    blueprints <- fromList <$> (maybeToList . parseBlueprint =<<) <$>
        lines <$> readFile "Day19/input.txt"
    let startRobots = fromList
            [("ore", 1), ("clay", 0), ("obsidian", 0), ("geode", 0)]
        startResources = fromList
            [("ore", 0), ("clay", 0), ("obsidian", 0), ("geode", 0)]
        start = [(startRobots, startResources)]

    let maxGeodes1 = M.map (findMax 24 start) blueprints
        qualityLevels = sum $ mapWithKey (*) maxGeodes1
    putStr "Sum of quality levels with 24 minutes: "
    print qualityLevels

    let maxGeodes2 = M.map (findMax 32 start) (M.take 3 blueprints)
        result2 = product maxGeodes2
    putStr "Product of max geodes of first 3 blueprints with 32 minutes: "
    print result2

    return (show qualityLevels, show result2)