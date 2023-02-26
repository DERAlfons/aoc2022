{-# LANGUAGE MultiWayIf #-}

module Day11.Main (main) where

import Data.List
import Data.Maybe (fromJust)

import My.Parser (parserRegex)
import qualified My.Parser as P

explode :: Eq a => a -> [a] -> [[a]]
explode _ [] = []
explode sep lst =
    let elm = takeWhile (/= sep) lst
        rst = dropWhile (/= sep) lst
    in case rst of
        [] -> [elm]
        (_ : rst2) -> elm : (explode sep rst2)

updateC :: ([Int], [[Int]]) -> (Int, (Int -> Int, Int -> Bool, Int, Int)) -> ([Int], [[Int]])
updateC (mCount, mItems) monkey @ (i, _) = (
    do
        (ci, cn) <- zip [0 ..] mCount
        return $ if ci == i
            then cn + length (mItems !! i)
            else cn,
    update mItems monkey)

update :: [[Int]] -> (Int, (Int -> Int, Int -> Bool, Int, Int)) -> [[Int]]
update mItems (this, (worry, check, left, right)) =
    let items = mItems !! this
        newItems = map worry items
        (leftItems, rightItems) = partition check newItems
    in do
        (i, oldItems) <- zip [0 ..] mItems
        return $ if
            | i == this -> []
            | i == left -> oldItems ++ leftItems
            | i == right -> oldItems ++ rightItems
            | otherwise -> oldItems

run :: [(Int -> Int, Int -> Bool, Int, Int)] -> [[Int]] -> [[Int]]
run monkeys mItems = foldl' update mItems $ zip [0 ..] monkeys

runC :: [(Int -> Int, Int -> Bool, Int, Int)] -> ([Int], [[Int]]) -> ([Int], [[Int]])
runC monkeys mItemsC = foldl' updateC mItemsC $ zip [0 ..] monkeys

parseItems :: String -> [Int]
parseItems = (fromJust .) $ P.run $ parserRegex "  Starting items: (.*)" $
    \[s] -> read $ "[" ++ s ++ "]"

parseWorry1 :: String -> (Int -> Int)
parseWorry1 = (fromJust .) $ P.run $ parserRegex "  Operation: new = old (.) (.*)" $
    \[op, arg] ->
        (\x ->
            (`div` 3) $
            (case op of
                "+" -> (+)
                "*" -> (*))
            (case arg of
                "old" -> x
                n -> read n :: Int)
            x)

parseWorry2 :: String -> (Int -> Int)
parseWorry2 = (fromJust .) $ P.run $ parserRegex "  Operation: new = old (.) (.*)" $
    \[op, arg] ->
        (\x ->
            (`mod` (2 * 3 * 5 * 7 * 11 * 13 * 17 * 19)) $
            (case op of
                "+" -> (+)
                "*" -> (*))
            (case arg of
                "old" -> x
                n -> read n :: Int)
            x)

parseCheck :: String -> (Int -> Bool)
parseCheck = (fromJust .) $ P.run $ parserRegex "  Test: divisible by (.*)" $
    \[n] -> (== 0) . (`mod` (read n))

parseLeft :: String -> Int
parseLeft = (fromJust .) $ P.run $ parserRegex "    If true: throw to monkey (.*)" $
    \[i] -> read i

parseRight :: String -> Int
parseRight = (fromJust .) $ P.run $ parserRegex "    If false: throw to monkey (.*)" $
    \[i] -> read i

parse1 :: [String] -> ([Int], (Int -> Int, Int -> Bool, Int, Int))
parse1 [_, i, w, c, l, r] = (parseItems i, (parseWorry1 w, parseCheck c, parseLeft l, parseRight r))

parse2 :: [String] -> ([Int], (Int -> Int, Int -> Bool, Int, Int))
parse2 [_, i, w, c, l, r] = (parseItems i, (parseWorry2 w, parseCheck c, parseLeft l, parseRight r))

main :: IO (String, String)
main = do
    (mItems1, monkeys1) <- unzip . map parse1 . explode "" . lines <$> readFile "Day11/input.txt"
    let (counts1, _) = (!! 20) $ iterate (runC monkeys1) $ unzip $ zip [0, 0 ..] mItems1
        monkeyBusiness1 = product $ take 2 $ reverse $ sort counts1
    putStrLn "Monkey business with decreasing worry level:"
    putStrLn $ show monkeyBusiness1
    (mItems2, monkeys2) <- unzip . map parse2 . explode "" . lines <$> readFile "Day11/input.txt"
    let (counts2, _) = (!! 10000) $ iterate (runC monkeys2) $ unzip $ zip [0, 0 ..] mItems2
        monkeyBusiness2 = product $ take 2 $ reverse $ sort counts2
    putStrLn "Monkey business with ever increasing worry level:"
    putStrLn $ show monkeyBusiness2
    return (show monkeyBusiness1, show monkeyBusiness2)