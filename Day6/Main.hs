module Day6.Main (main) where

import Data.List (nub)

import My.Util (maybeToIO)

distinct :: Eq a => [a] -> Bool
distinct bs = length (nub bs) == length bs

getMarkerOffset :: Int -> String -> Maybe Int
getMarkerOffset markerLength stream = (+ markerLength) <$> loop 0 stream
    where
    loop i s
        | length s < markerLength = Nothing
        | distinct (take markerLength s) = Just i
        | otherwise = loop (i + 1) (tail s)

getPacketOffset :: String -> Maybe Int
getPacketOffset = getMarkerOffset 4

getMsgOffset :: String -> Maybe Int
getMsgOffset = getMarkerOffset 14

main :: IO (String, String)
main = do
    buffer <- readFile "Day6/input.txt"

    packetOffset <- maybeToIO "No packet found" $ getPacketOffset buffer
    putStr "Packet offset: "
    print packetOffset

    msgOffset <- maybeToIO "No msg found" $ getMsgOffset buffer
    putStr "Msg offset: "
    print msgOffset

    return (show packetOffset, show msgOffset)