{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.FilePath ((</>))
import System.Exit (exitSuccess, exitFailure)

import GenMains (genMains)

import qualified Day1.Main

mains :: [(String, IO (String, String))]
mains = $genMains

main :: IO ()
main = do -- IO
    results <- sequence $ do -- []
        (day, m) <- mains
        return $ do -- IO
            (answer1, answer2) <- m
            [check1, check2] <- lines <$> readFile (day </> "check.txt")
            putStrLn $ day ++ " part 1: " ++ if answer1 == check1 then "SUCCESS" else "FAILURE"
            putStrLn $ day ++ " part 2: " ++ if answer2 == check2 then "SUCCESS" else "FAILURE"
            return $ answer1 == check1 && answer2 == check2
    if and results then
        exitSuccess
    else
        exitFailure