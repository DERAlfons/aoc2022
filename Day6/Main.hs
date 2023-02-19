module Day6.Main (main) where

import Data.List

getMarkerOffset :: Int -> String -> Int
getMarkerOffset markerLength s =
    let (initSegment, restS) = splitAt markerLength s
    in loop markerLength initSegment restS
    where
    loop i segment rest
        | length (nub segment) == markerLength = i
        | otherwise = loop (i + 1) ((tail segment) ++ [head rest]) (tail rest)

getPacketOffset :: String -> Int
getPacketOffset = getMarkerOffset 4

getMsgOffset :: String -> Int
getMsgOffset = getMarkerOffset 14

main :: IO (String, String)
main = do
    buffer <- readFile "Day6/input.txt"
    let packetOffset = getPacketOffset buffer
    putStrLn "Packet offset:"
    putStrLn $ show packetOffset
    let msgOffset = getMsgOffset buffer
    putStrLn "Msg offset:"
    putStrLn $ show msgOffset
    return (show packetOffset, show msgOffset)