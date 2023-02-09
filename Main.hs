{-# LANGUAGE TemplateHaskell #-}

module Main where

import RefList (sList)

import qualified Day1.Main

$(sList ["Day1"])

main :: IO ()
main = do
    putStrLn "Running solutions to Day1 ..."
    slns !! 0
    return ()