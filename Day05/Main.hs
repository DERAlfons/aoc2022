module Day05.Main (main) where

import Data.List (transpose, foldl')
import Data.Map (Map, fromList, (!), adjust, toAscList)
import Data.Maybe (maybeToList)

import My.Util (explode)
import My.Parser (parserRegex, run)

data Instruction = Instruction Int Int Int

move :: ([Char] -> [Char]) -> Map Int [Char] -> Instruction -> Map Int [Char]
move tf stacks (Instruction n src dst) =
    let crates = tf $ take n $ stacks ! src
        newStacks = adjust (drop n) src stacks in
    adjust (crates ++) dst newStacks

toStack :: [Char] -> (Int, [Char])
toStack s =
    let id : stack = takeWhile (/= ' ') s in
    (read [id], reverse stack)

parseInstruction :: String -> Maybe Instruction
parseInstruction = run $ parserRegex
    "move (\\d+) from (\\d+) to (\\d+)$"
    (\[n, src, dst] -> Instruction (read n) (read src) (read dst))

main :: IO (String, String)
main = do
    [sCrates, sInstructions] <- explode "" . lines <$> readFile "Day05/input.txt"
    let startCrateStacks = fromList $ map toStack $
            filter ((`elem` ['1' .. '9']) . head) $
            transpose $ reverse sCrates
        instructions = maybeToList . parseInstruction =<< sInstructions

    let endCrateStacks1 = foldl' (move reverse) startCrateStacks instructions
        endCrates1 = map (head . snd) $ toAscList endCrateStacks1
    putStr "Top crates with old instructions: "
    putStrLn endCrates1

    let endCrateStacks2 = foldl' (move id) startCrateStacks instructions
        endCrates2 = map (head . snd) $ toAscList endCrateStacks2
    putStr "Top crates with new instructions: "
    putStrLn endCrates2

    return (endCrates1, endCrates2)