{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import GenMains (genMains)

import qualified Day01.Main
import qualified Day02.Main
import qualified Day03.Main
import qualified Day04.Main
import qualified Day05.Main
import qualified Day05v2.Main
import qualified Day06.Main
import qualified Day07.Main
import qualified Day08.Main
import qualified Day08v0.Main
import qualified Day09.Main
import qualified Day10.Main
import qualified Day11.Main
import qualified Day12.Main
import qualified Day13.Main
import qualified Day14.Main
import qualified Day15.Main
import qualified Day16.Main
import qualified Day17.Main
import qualified Day18.Main
import qualified Day19.Main
import qualified Day20.Main
import qualified Day20v2.Main
import qualified Day21.Main
import qualified Day22.Main
import qualified Day23.Main
import qualified Day24.Main
import qualified Day25.Main

mains :: [(String, IO (String, String))]
mains = $genMains

main :: IO ()
main = do -- IO
    args <- getArgs
    let testMains = case args of
            [] -> mains
            otherwise -> filter ((`elem` args) . fst) mains
    results <- sequence $ do -- []
        (day, m) <- testMains
        return $ do -- IO
            (answer1, answer2) <- m
            [check1, check2] <- lines <$> readFile (day ++ "/check.txt")
            putStrLn $ day ++ " part 1: " ++ if answer1 == check1 then "SUCCESS" else "FAILURE"
            putStrLn $ day ++ " part 2: " ++ if answer2 == check2 then "SUCCESS" else "FAILURE"
            return $ answer1 == check1 && answer2 == check2
    if and results then
        exitSuccess
    else
        exitFailure