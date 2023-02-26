module Day11.Main (main) where

import Data.Function ((&))
import Data.List (foldl', foldl1', partition)
import Data.Maybe (fromJust)
import Data.Map (Map, fromList, (!), size, insert, adjust, elems)

import My.Util (maybeToIO, sortDesc)
import My.Parser (Parser, parserRegex, parserList, run)

data Monkey = Monkey {
    inspectionCount :: Int,
    items :: [Int],
    updateWorry :: Int -> Int,
    checkM :: Int,
    dstTrue :: Int,
    dstFalse :: Int}

addItems :: [Int] -> Monkey -> Monkey
addItems newItems monkey =
    let oldItems = items monkey in
    monkey {items = oldItems ++ newItems}

turn :: (Int -> Int) -> Map Int Monkey -> Int -> Map Int Monkey
turn reduceWorry monkeys i =
    let m = monkeys ! i
        inspectedItems = map (reduceWorry . updateWorry m) (items m)
        newInspectionCount = inspectionCount m + length inspectedItems
        (trueItems, falseItems) = partition (\x -> x `mod` checkM m == 0)
            inspectedItems in
    foldl' (&) monkeys [
        insert i m {inspectionCount = newInspectionCount, items = []},
        adjust (addItems trueItems) (dstTrue m),
        adjust (addItems falseItems) (dstFalse m)]

round1 :: Map Int Monkey -> Map Int Monkey
round1 monkeys = foldl' (turn (`div` 3)) monkeys [0 .. size monkeys - 1]

round2 :: Int -> Map Int Monkey -> Map Int Monkey
round2 modulus monkeys = foldl' (turn (`mod` modulus)) monkeys
    [0 .. size monkeys - 1]

parserMonkey :: Parser Monkey
parserMonkey = Monkey 0 <$>
    parserList "\\s*Starting items: " ", " "\\n" (
        parserRegex "(\\d+)" $ \[n] -> read n) <*>
    parserRegex "\\s*Operation: new = old (.) (.+)\\n" (
        \[op, arg] -> case (op, arg) of
            ("*", "old") -> (^ 2)
            ("*", v) -> (* read v)
            ("+", v) -> (+ read v)) <*>
    parserRegex "\\s*Test: divisible by (\\d+)\\n" (
        \[n] -> read n) <*>
    parserRegex "\\s*If true: throw to monkey (\\d+)\\n" (
        \[i] -> read i) <*>
    parserRegex "\\s*If false: throw to monkey (\\d+)\\n" (
        \[i] -> read i)

parseMonkeys :: String -> Maybe (Map Int Monkey)
parseMonkeys = run $ fmap fromList $ parserList "" "\\n" "$" $ (,) <$>
    parserRegex "Monkey (\\d+):\\n" (\[n] -> read n) <*>
    parserMonkey

main :: IO (String, String)
main = do
    monkeys <- maybeToIO "error while parsing monkeys" . parseMonkeys =<<
        readFile "Day11/input.txt"

    let newMonkeys1 = (!! 20) $ iterate round1 monkeys
        inspectionCounts1 = map inspectionCount $ elems newMonkeys1
        monkeyBusiness1 = product $ take 2 $ sortDesc inspectionCounts1
    putStr "Monkey business with decreasing worry level: "
    print monkeyBusiness1

    let modulus = foldl1' lcm $ map checkM $ elems monkeys
        newMonkeys2 = (!! 10000) $ iterate (round2 modulus) monkeys
        inspectionCounts2 = map inspectionCount $ elems newMonkeys2
        monkeyBusiness2 = product $ take 2 $ sortDesc inspectionCounts2
    putStr "Monkey business with ever increasing worry level: "
    print monkeyBusiness2

    return (show monkeyBusiness1, show monkeyBusiness2)