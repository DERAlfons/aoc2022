{-# LANGUAGE CApiFFI #-}

module Day08v0.Main (main) where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (newArray)

foreign import capi "lines_of_sight.h countVisibleTrees" countVisibleTrees :: Ptr CInt -> CInt -> CInt -> CInt
foreign import capi "lines_of_sight.h getMaxScenicScore" getMaxScenicScore :: Ptr CInt -> CInt -> CInt -> CInt

main :: IO (String, String)
main = do
    trees <- map (map (read . return)) . lines <$> readFile "Day08v0/input.txt"
    pTrees <- newArray $ concat trees
    let height = fromIntegral $ length trees
        width = fromIntegral $ length $ head trees

    let visibleTrees = countVisibleTrees pTrees height width
    putStr "Visible trees: "
    print visibleTrees

    let maxScenicScore = getMaxScenicScore pTrees height width
    putStr "Maximum scenic score: "
    print maxScenicScore

    return (show visibleTrees, show maxScenicScore)