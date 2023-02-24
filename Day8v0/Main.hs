{-# LANGUAGE CApiFFI #-}

module Day8v0.Main (main) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

foreign import capi "vis.h vis_count" vis_count :: Ptr CInt -> CInt -> CInt -> CInt
foreign import capi "vis.h max_scenic_score" max_scenic_score :: Ptr CInt -> CInt -> CInt -> CInt

main :: IO (String, String)
main = do
    trees <- map (map (read . return)) <$> lines <$> readFile "Day8v0/input.txt"
    let height = fromIntegral $ length trees
        width = fromIntegral $ length $ head trees
    ptrees <- newArray $ concat trees
    putStrLn "Visible trees:"
    putStrLn $ show $ vis_count ptrees height width
    putStrLn "Maximum scenic score:"
    putStrLn $ show $ max_scenic_score ptrees height width
    return (show $ vis_count ptrees height width, show $ max_scenic_score ptrees height width)
