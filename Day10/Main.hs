module Day10.Main (main) where

import Data.List
import Data.Maybe (fromJust)
import Data.Foldable (asum)

import My.Parser (parserRegex, run)

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n bs =
    let (g, rest) = splitAt n bs
    in g : groupsOf n rest

parse :: String -> Maybe [Int -> Int]
parse = run $ asum [
    parserRegex "noop" $ \[] -> [id],
    parserRegex "addx (-?\\d+)" $ \[x] -> [id, (+ (read x))]]

main :: IO (String, String)
main = do
    instructions <- concat . map (fromJust . parse) . lines <$> readFile "Day10/input.txt"
    let xReg = scanl' (flip ($)) 1 instructions
        signal = zipWith (*) [1 ..] xReg
        result = sum $ map (signal !!) [19, 59 .. 219]
    putStrLn "Sum of signals:"
    putStrLn $ show result
    putStrLn "Display output:"
    sequence $ do
        xLine <- groupsOf 40 xReg
        let drawLine = zipWith (\pos x -> abs (x - pos) <= 1) [0 ..] xLine
        return $ putStrLn $ map (\p -> if p then '#' else '.') drawLine
    return (show result, "")