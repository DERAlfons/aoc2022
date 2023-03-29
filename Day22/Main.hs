{-# LANGUAGE MultiWayIf #-}

module Day22.Main (main) where

import Data.List (elemIndex, foldl')
import Data.Array (Array, listArray, bounds, (!))
import Data.Foldable (asum)
import Control.Applicative (many)
import Control.Monad (guard)

import My.Util (explode, maybeToIO)
import My.Parser (parserRegex, run)

type Point = (Int, Int)
type Position = (Point, Char)
data Instruction = Move Int | Turn Char

walk :: (Position -> Maybe Position) -> Position -> Instruction -> Position
walk _ (pt, d) (Turn t) = (pt, turn t d)
walk _ pos (Move 0) = pos
walk step pos (Move w) = case step pos of
    Just newPos -> walk step newPos (Move (w - 1))
    Nothing -> pos

turn :: Char -> Char -> Char
turn 'L' 'N' = 'W'
turn 'L' 'E' = 'N'
turn 'L' 'S' = 'E'
turn 'L' 'W' = 'S'
turn 'R' 'N' = 'E'
turn 'R' 'E' = 'S'
turn 'R' 'S' = 'W'
turn 'R' 'W' = 'N'

stepFlat :: Array Point Char -> Position -> Maybe Position
stepFlat m ((y, x), d) = let
    (maxY, maxX) = snd $ bounds m
    newPt = case d of
        'N' -> ((y - 1) `mod` (maxY + 1), x)
        'E' -> (y, (x + 1) `mod` (maxX + 1))
        'S' -> ((y + 1) `mod` (maxY + 1), x)
        'W' -> (y, (x - 1) `mod` (maxX + 1)) in
    case m ! newPt of
        ' ' -> stepFlat m (newPt, d)
        '.' -> Just (newPt, d)
        '#' -> Nothing

stepCube :: Array Point Char -> Position -> Maybe Position
stepCube m pos @ ((y, x), d) = let
    (newY, newX) = case d of
        'N' -> (y - 1, x)
        'E' -> (y, x + 1)
        'S' -> (y + 1, x)
        'W' -> (y, x - 1)
    tfPos @ (tfPt, _) = if
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
        | otherwise -> ((newY, newX), d) in
    guard (m ! tfPt == '.') >> return tfPos

parsePath :: String -> Maybe [Instruction]
parsePath = run $ many $ asum [
    parserRegex "(\\d+)" $ \[n] -> Move $ read n,
    parserRegex "(L|R)" $ \[d] -> Turn $ head d]

directionValue :: Char -> Int
directionValue 'E' = 0
directionValue 'S' = 1
directionValue 'W' = 2
directionvalue 'N' = 3

main :: IO (String, String)
main = do
    [mLines, [sPath]] <- explode "" . lines <$> readFile "Day22/input.txt"
    let lenY = length mLines
        lenX = maximum $ map length mLines
        mLinesPad = map (take lenX . (++ repeat ' ')) mLines
        mArr = listArray ((0, 0), (lenY - 1, lenX - 1)) $ concat mLinesPad
        Just startX = elemIndex '.' (mLines !! 0)
    path <- maybeToIO "error while parsing movement instructions" $ parsePath sPath

    let ((rY, rX), rD) = foldl' (walk (stepFlat mArr)) ((0, startX), 'E') path
        result = 1000 * (rY + 1) + 4 * (rX + 1) + directionValue rD
    putStr "Code of final position: "
    print result

    let ((rYc, rXc), rDc) = foldl' (walk (stepCube mArr)) ((0, startX), 'E') path
        resultC = 1000 * (rYc + 1) + 4 * (rXc + 1) + directionValue rDc
    putStr "Code of final position on cube: "
    print resultC

    return (show result, show resultC)