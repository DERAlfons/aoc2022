{-# LANGUAGE CApiFFI #-}

module Day20v2.Main (main) where

import Data.List (elemIndex)

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (newArray, peekArray)

foreign import capi "mix.h mix" mix :: Ptr Int -> CInt -> CInt -> Ptr Int

getCoords :: [Int] -> [Int]
getCoords a = [a !! ((i0 + i) `mod` length a) | i <- [1000, 2000, 3000]]
    where
    Just i0 = elemIndex 0 a

main :: IO (String, String)
main = do
    coords <- map read . lines <$> readFile "Day20v2/input.txt"
    let len = length coords

    pCoords <- newArray coords
    result1 <- fmap (sum . getCoords) $ peekArray len $ mix pCoords (fromIntegral len) 1
    putStr "Sum of coordinates without decryption key and with 1 round of mixing: "
    print result1

    pCoordsD <- newArray $ map (* 811589153) coords
    result2 <- fmap (sum . getCoords) $ peekArray len $ mix pCoordsD (fromIntegral len) 10
    putStr "Sum of coordinates with decryption key and with 10 rounds of mixing: "
    print result2

    return (show result1, show result2)