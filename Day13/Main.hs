module Day13.Main (main) where

import Data.List (elemIndex, elemIndices, sort)
import Data.Maybe (maybeToList)
import Data.Foldable (asum)

import My.Util (explode)
import My.Parser (Parser, parserRegex, parserList, run)

data Packet = Value Int | List [Packet] deriving Eq

instance Ord Packet where
    compare (Value a) (Value b) = compare a b
    compare (Value a) (List  b) = compare (List [Value a]) (List b)
    compare (List  a) (Value b) = compare (List a) (List [Value b])
    compare (List  a) (List  b) = compare a b

parserPacket :: Parser Packet
parserPacket = asum [
    parserRegex "(\\d+)" (\[v] -> Value (read v)),
    List <$> parserList "\\[" "," "\\]" parserPacket]

main :: IO (String, String)
main = do
    packetPairs <- map (maybeToList . run parserPacket =<<) <$>
        explode "" . lines <$> readFile "Day13/input.txt"

    let result = sum $ map (+ 1) $ elemIndices True $
            map (\[p1, p2] -> p1 < p2) packetPairs
    putStr "Sum of correctly ordered packet pairs: "
    print result

    let packets = concat packetPairs
        d1 = List [List [Value 2]]
        d2 = List [List [Value 6]]
        sortedPackets = sort $ d1 : d2 : packets
        Just i1 = elemIndex d1 sortedPackets
        Just i2 = elemIndex d2 sortedPackets
        decoderKey = (i1 + 1) * (i2 + 1)
    putStr "Decoder key: "
    print decoderKey

    return (show result, show decoderKey)