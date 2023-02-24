module Day10.Main (main) where

import Data.List (scanl')
import Data.Maybe (maybeToList)
import Data.Foldable (asum)

import My.Util (groupsOf)
import My.Parser (parserRegex, run)

parse :: String -> Maybe [Int -> Int]
parse = run $ asum [
    parserRegex "noop" $ \[] -> [id],
    parserRegex "addx (-?\\d+)" $ \[x] -> [id, (+ (read x))]]

main :: IO (String, String)
main = do
    instructions <- (concat . maybeToList . parse =<<) . lines <$>
        readFile "Day10/input.txt"
    let xReg = scanl' (flip ($)) 1 instructions

    let signal = zipWith (*) [1 ..] xReg
        result = sum $ map (signal !!) [19, 59 .. 219]
    putStr "Sum of signals: "
    print result

    let displayLines = do
            xLine <- groupsOf 40 xReg
            let drawLine = zipWith (\pos x -> abs (x - pos) <= 1) [0 ..] xLine
            return $ map (\p -> if p then '#' else '.') drawLine
    putStrLn "Display output:"
    sequence $ map putStrLn displayLines

    return (show result, concat displayLines)