module Day20.Main (main) where

import Data.List (elemIndex)
import Control.Monad (forM_, replicateM)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, newListArray, readArray, writeArray)

import My.Util (for, applyN)

data Node = Node {value :: Int, prev :: Int, next :: Int}

rmv :: STArray s Int Node -> Node -> ST s ()
rmv a n = do
    before <- readArray a (prev n)
    after <- readArray a (next n)
    writeArray a (prev n) before {next = next n}
    writeArray a (next n) after {prev = prev n}

ins :: STArray s Int Node -> Int -> Node -> Int -> ST s ()
ins a i n iAfter = do
    after <- readArray a iAfter
    before <- readArray a (prev after)
    writeArray a (prev after) before {next = i}
    writeArray a (next before) after {prev = i}
    writeArray a i n {prev = prev after, next = next before}

mix :: STArray s Int Node -> Int -> ST s ()
mix a len = forM_ [0 .. len - 1] $ \i -> do
    n <- readArray a i
    let shift = value n `mod` (len - 1)
    iAfter <- applyN shift (fmap next . readArray a =<<) (return $ next n)
    rmv a n
    ins a i n iAfter

mixN :: Int -> [Int] -> [Int]
mixN n list = map value $ runST $ do
    let len = length list
        nodeList = for (zip [0 ..] list) $ \(i, v) ->
            Node v ((i - 1) `mod` len) ((i + 1) `mod` len)
    a <- newListArray (0, len - 1) nodeList
    replicateM n $ mix a len
    sequence $ take len $ iterate (readArray a . next =<<) (readArray a 0)

getCoords :: [Int] -> [Int]
getCoords a = [a !! ((i0 + i) `mod` length a) | i <- [1000, 2000, 3000]]
    where
    Just i0 = elemIndex 0 a

main :: IO (String, String)
main = do
    coordinates <- map read . lines <$> readFile "Day20/input.txt"
    let len = length coordinates

    let mcs = mixN 1 coordinates
        result1 = sum $ getCoords mcs
    putStr "Sum of coordinates without decryption key and with 1 round of mixing: "
    print result1

    let mcsd = mixN 10 (map (* 811589153) coordinates)
        result2 = sum $ getCoords mcsd
    putStr "Sum of coordinates with decryption key and with 10 rounds of mixing: "
    print result2

    return (show result1, show result2)