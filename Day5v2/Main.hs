module Day5v2.Main (main) where

import Data.List (transpose, foldl')
import Data.Maybe (maybeToList)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray, newListArray, readArray, writeArray, runSTArray)
import Data.Array (elems)

import My.Util (explode)
import My.Parser (parserRegex, run)

data Instruction = Instruction Int Int Int deriving Show

move :: ([Char] -> [Char]) -> ST s (STArray s Int [Char]) -> Instruction -> ST s (STArray s Int [Char])
move tf stStacks (Instruction n src dst) = do
    stacks <- stStacks
    srcCrates <- readArray stacks src
    let (moveCrates, remainCrates) = splitAt n srcCrates
    writeArray stacks src remainCrates
    dstCrates <- readArray stacks dst
    writeArray stacks dst (tf moveCrates ++ dstCrates)
    return stacks

toStack :: [Char] -> [Char]
toStack s = dropWhile (== ' ') $ reverse $ tail s

parseInstruction :: String -> Maybe Instruction
parseInstruction = run $ parserRegex
    "move (\\d+) from (\\d+) to (\\d+)$"
    (\[n, src, dst] -> Instruction (read n) (read src) (read dst))

main :: IO (String, String)
main = do
    [sCrates, sInstructions] <- explode "" . lines <$> readFile "Day5v2/input.txt"
    let startCrateStacks = newListArray (1, 9) $ map toStack $
            filter ((`elem` ['1' .. '9']) . head) $
            transpose $ reverse sCrates
            :: ST s (STArray s Int [Char])
        instructions = maybeToList . parseInstruction =<< sInstructions

    let endCrateStacks1 = runSTArray $ foldl' (move reverse) startCrateStacks instructions
        endCrates1 = map head $ elems endCrateStacks1
    putStr "Top crates with old instructions: "
    putStrLn endCrates1

    let endCrateStacks2 = runSTArray $ foldl' (move id) startCrateStacks instructions
        endCrates2 = map head $ elems endCrateStacks2
    putStr "Top crates with new instructions: "
    putStrLn endCrates2

    return (endCrates1, endCrates2)