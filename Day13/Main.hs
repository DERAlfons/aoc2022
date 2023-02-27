module Day13.Main (main) where

import Data.List
import Data.Maybe
import Data.Foldable (asum)

import My.Util (explode)
import My.Parser

data Packet = Value Int | List [Packet] deriving (Eq, Show)

instance Ord Packet where
    compare (Value a) (Value b) = compare a b
    compare (Value a) (List b) = compare (List [Value a]) (List b)
    compare (List a) (Value b) = compare (List a) (List [Value b])
    compare (List a) (List b) = compare a b

parsePacket :: Parser Packet
parsePacket = asum [
    parserRegex "([0-9]+)" (\[v] -> Value (read v)),
    List <$> parserList "\\[" "," "\\]" parsePacket]

parse :: String -> Packet
parse = fromJust . run parsePacket

main :: IO (String, String)
main = do
    packetPairs <- map (map parse) . explode "" . lines <$> readFile "Day13/input.txt"
    let result = sum $ map (+ 1) $ elemIndices True $ map (\[p1, p2] -> p1 < p2) packetPairs
    putStrLn "Sum of correctly ordered packet pairs:"
    putStrLn $ show result
    let packets = concat packetPairs
        d1 = parse "[[2]]"
        d2 = parse "[[6]]"
        sPackets = sort $ d1 : d2 : packets
        Just i1 = elemIndex d1 sPackets
        Just i2 = elemIndex d2 sPackets
        decoder = (i1 + 1) * (i2 + 1)
    putStrLn "Decoder key:"
    putStrLn $ show decoder
    return (show result, show decoder)