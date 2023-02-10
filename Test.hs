module Main where

import System.Exit (exitFailure)

import Day1.Main (answer1, answer2)

main :: IO ()
main = do
    input <- readFile "Day1/input.txt"
    [a1, a2] <- lines <$> readFile "Day1/answer.txt"
    if answer1 input == a1 && answer2 input == a2 then
        return ()
    else
        exitFailure