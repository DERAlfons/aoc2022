{-# LANGUAGE MultiWayIf #-}

module Day22.Main (main) where

import Data.List
import Data.Maybe
import Data.Array

import My.Parser

walk :: Array (Int, Int) Char
     -> (Array (Int, Int) Char -> (Int, Int) -> Char -> ((Int, Int), Char))
     -> (Int, Int) -> [Int] -> Char -> [Char] -> ((Int, Int), Char)
walk _ _ pos [] d _ = (pos, d)
walk _ _ pos [0] d _ = (pos, d)
walk m step pos (0 : ws) d (t : ts) = walk m step pos ws (turn t d) ts
walk m step pos (w : ws) d ts =
    let (newPos, newD) = step m pos d
    in if m ! newPos == '#' then
        case ts of
            [] -> (pos, d)
            t : tss -> walk m step pos ws (turn t d) tss
    else
        walk m step newPos ((w - 1) : ws) newD ts

turn :: Char -> Char -> Char
turn 'L' 'N' = 'W'
turn 'L' 'E' = 'N'
turn 'L' 'S' = 'E'
turn 'L' 'W' = 'S'
turn 'R' 'N' = 'E'
turn 'R' 'E' = 'S'
turn 'R' 'S' = 'W'
turn 'R' 'W' = 'N'

stepFlat :: Array (Int, Int) Char -> (Int, Int) -> Char -> ((Int, Int), Char)
stepFlat m (posY, posX) d =
    let (maxY, maxX) = snd $ bounds m
        newPos = case d of
            'N' -> ((posY - 1) `mod` (maxY + 1), posX)
            'E' -> (posY, (posX + 1) `mod` (maxX + 1))
            'S' -> ((posY + 1) `mod` (maxY + 1), posX)
            'W' -> (posY, (posX - 1) `mod` (maxX + 1))
    in if m ! newPos == ' ' then
        stepFlat m newPos d
    else
        (newPos, d)

stepCube :: Array (Int, Int) Char -> (Int, Int) -> Char -> ((Int, Int), Char)
stepCube m (posY, posX) d =
    let (newY, newX) = case d of
            'N' -> (posY - 1, posX)
            'E' -> (posY, posX + 1)
            'S' -> (posY + 1, posX)
            'W' -> (posY, posX - 1)
    in if
        | d == 'N' && newY == -1 && newX >= 50 && newX <= 99 -> ((100 + newX, 0), 'E')
        | d == 'N' && newY == -1 && newX >= 100 && newX <= 149 -> ((199, newX - 100), 'N')
        | d == 'W' && newX == 49 && newY >= 0 && newY <= 49 -> ((149 - newY, 0), 'E')
        | d == 'E' && newX == 150 && newY >= 0 && newY <= 49 -> ((149 - newY, 99), 'W')
        | d == 'S' && newY == 50 && newX >= 100 && newX <= 149 -> ((newX - 50, 99), 'W')
        | d == 'W' && newX == 49 && newY >= 50 && newY <= 99 -> ((100, newY - 50), 'S')
        | d == 'E' && newX == 100 && newY >= 50 && newY <= 99 -> ((49, newY + 50), 'N')
        | d == 'N' && newY == 99 && newX >= 0 && newX <= 49 -> ((newX + 50, 50), 'E')
        | d == 'W' && newX == -1 && newY >= 100 && newY <= 149 -> ((149 - newY, 50), 'E')
        | d == 'E' && newX == 100 && newY >= 100 && newY <= 149 -> ((149 - newY, 149), 'W')
        | d == 'S' && newY == 150 && newX >= 50 && newX <= 99 -> ((100 + newX, 49), 'W')
        | d == 'W' && newX == -1 && newY >= 150 && newY <= 199 -> ((0, newY - 100), 'S')
        | d == 'E' && newX == 50 && newY >= 150 && newY <= 199 -> ((149, newY - 100), 'N')
        | d == 'S' && newY == 200 && newX >= 0 && newX <= 49 -> ((0, newX + 100), 'S')
        | otherwise -> ((newY, newX), d)

parserW :: Parser [Int]
parserW = parserList "" "R|L" "$" $ parserRegex "([0-9]+)" (\[n] -> read n)

parse :: String -> [Int]
parse = fromJust . run parserW

directionValue :: Char -> Int
directionValue 'E' = 0
directionValue 'S' = 1
directionValue 'W' = 2
directionvalue 'N' = 3

main :: IO (String, String)
main = do
    (mLines, [_, insts]) <- break (== "") . lines <$> readFile "Day22/input.txt"
    let lengthY = length mLines
        lengthX = length (mLines !! 0)
        Just startX = elemIndex '.' (mLines !! 0)
        emLines = map (\x -> take lengthX $ x ++ (repeat ' ')) mLines
        m = listArray ((0, 0), (lengthY - 1, lengthX - 1)) $ concat emLines
        ws = parse insts
        ts = filter (\x -> x == 'R' || x == 'L') insts
        ((rY, rX), rD) = walk m stepFlat (0, startX) ws 'E' ts
        result = 1000 * (rY + 1) + 4 * (rX + 1) + directionValue rD
    putStrLn "Code of final position:"
    putStrLn $ show result
    let ((rYc, rXc), rDc) = walk m stepCube (0, startX) ws 'E' ts
        resultC = 1000 * (rYc + 1) + 4 * (rXc + 1) + directionValue rDc
    putStrLn "Code of final position on cube:"
    putStrLn $ show resultC
    return (show result, show resultC)