module Day20.Main (main) where

import Data.List
import Data.Array
import Data.Array.ST
import Control.Monad.ST

mix :: Int -> [Int] -> Array Int (Int, Int)
mix n list = runSTArray $ do
    let len = length list
    a1 <- newListArray (0, len - 1) $ zip list [0, 1 ..] :: ST s (STArray s Int (Int, Int))
    a2 <- newListArray (0, len - 1) [0, 1 ..] :: ST s (STArray s Int Int)
    sequence $ concat $ replicate n $ do
        i <- [0 .. len - 1]
        return $ do
            (steps, pos) <- readArray a1 i
            let newPos = pos + (steps `mod` (len - 1))
            writeArray a1 i (steps, newPos `mod` len)
            sequence $ do
                j <- map (`mod` len) [pos + 1 .. newPos]
                return $ do
                    p1 <- readArray a2 j
                    (s2, p2) <- readArray a1 p1
                    writeArray a1 p1 (s2, (p2 - 1) `mod` len)
                    writeArray a2 ((j - 1) `mod` len) p1
            writeArray a2 (newPos `mod` len) i
    return a1

main :: IO (String, String)
main = do
    coordinates <- map read . lines <$> readFile "Day20/input.txt"
    let len = length coordinates
        mcsarr = mix 1 coordinates
        mcs = map fst $ sortOn snd $ elems mcsarr
        Just i0 = elemIndex 0 mcs
        c1 = mcs !! ((i0 + 1000) `mod` len)
        c2 = mcs !! ((i0 + 2000) `mod` len)
        c3 = mcs !! ((i0 + 3000) `mod` len)
    putStrLn "Sum of coordinates without decryption key and with 1 round of mixing:"
    putStrLn $ show $ c1 + c2 + c3
    let mcsarrd = mix 10 (map (* 811589153) coordinates)
        mcsd = map fst $ sortOn snd $ elems mcsarrd
        Just i0d = elemIndex 0 mcsd
        c1d = mcsd !! ((i0d + 1000) `mod` len)
        c2d = mcsd !! ((i0d + 2000) `mod` len)
        c3d = mcsd !! ((i0d + 3000) `mod` len)
    putStrLn "Sum of coordinates with decryption key and with 10 rounds of mixing:"
    putStrLn $ show $ c1d + c2d + c3d
    return (show $ c1 + c2 + c3, show $ c1d + c2d + c3d)