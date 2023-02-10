module Main where

import System.Exit (exitFailure)

import Paths_aoc2022 (getDataFileName)
import Day1.Main (answer1, answer2)

main :: IO ()
main = do
    input <- readFile =<< getDataFileName "Day1/input.txt"
    [a1, a2] <- lines <$> (readFile =<< getDataFileName "Day1/answer.txt")
    if answer1 input == a1 && answer2 input == a2 then
        return ()
    else
        exitFailure