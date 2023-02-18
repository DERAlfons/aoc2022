module Day4.Main (main) where

import Data.Maybe (maybeToList)

import My.Util (count)
import My.Parser (parserRegex, run)

data Range = Range {start :: Int, end :: Int}

contains :: Range -> Range -> Bool
contains r1 r2 =
    (start r1 <= start r2 && end r2 <= end r1) ||
    (start r2 <= start r1 && end r1 <= end r2)

overlaps :: Range -> Range -> Bool
overlaps r1 r2 = start r1 <= end r2 && start r2 <= end r1

parseRanges :: String -> Maybe (Range, Range)
parseRanges = run $ parserRegex
    "(\\d+)-(\\d+),(\\d+)-(\\d+)$"
    (\[s1, e1, s2, e2] -> (Range (read s1) (read e1), Range (read s2) (read e2)))

main :: IO (String, String)
main = do
    rangePairs <- concat . map (maybeToList . parseRanges) . lines <$> readFile "Day4/input.txt"

    let containedCount = count (uncurry contains) rangePairs
    putStr "Number of pairs with fully contained area: "
    print containedCount

    let overlapCount = count (uncurry overlaps) rangePairs
    putStr "Number of pairs with overlap: "
    print overlapCount

    return (show containedCount, show overlapCount)