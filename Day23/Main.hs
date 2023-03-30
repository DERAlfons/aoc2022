module Day23.Main (main) where

import Data.Set (Set, fromList, notMember)
import qualified Data.Set as S
import Data.Map (Map, fromListWith)
import qualified Data.Map as M
import Control.Monad

run :: Set (Int, Int) -> Int
run elves = loop 1 0 elves
    where
    loop n rot elves =
        let (newElves, finished) = step elves rot
        in if finished then
               n
           else
               loop (n + 1) ((rot + 1) `mod` 4) newElves

update :: Int -> Set (Int, Int) -> Set (Int, Int)
update n elves = loop n 0 elves
    where
    loop 0 _ elves = elves
    loop n rot elves = loop (n - 1) ((rot + 1) `mod` 4) (fst $ step elves rot)

step :: Set (Int, Int) -> Int -> (Set (Int, Int), Bool)
step elves rot =
    let props = M.toList $ fromListWith (++) $ do
            e <- S.toList elves
            return (getProp elves rot e, [e])
        newElves = fromList $ do
            (p, es) <- props
            case es of
                [_] -> return p
                otherwise -> es
        finished = all (\(p, es) -> es == [p]) props
    in (newElves, finished)

getProp :: Set (Int, Int) -> Int -> (Int, Int) -> (Int, Int)
getProp elves rot (ex, ey) =
    let props = [
            if all (flip notMember elves) [(ex - 1, ey - 1), (ex, ey - 1), (ex + 1, ey - 1)] then [(ex, ey - 1)] else [],
            if all (flip notMember elves) [(ex - 1, ey + 1), (ex, ey + 1), (ex + 1, ey + 1)] then [(ex, ey + 1)] else [],
            if all (flip notMember elves) [(ex - 1, ey - 1), (ex - 1, ey), (ex - 1, ey + 1)] then [(ex - 1, ey)] else [],
            if all (flip notMember elves) [(ex + 1, ey - 1), (ex + 1, ey), (ex + 1, ey + 1)] then [(ex + 1, ey)] else []]
        rProps = concat $ (drop rot props) ++ (take rot props)
    in case length rProps of
        0 -> (ex, ey)
        4 -> (ex, ey)
        otherwise -> head rProps

main :: IO (String, String)
main = do
    m <- lines <$> readFile "Day23/input.txt"
    let elves = fromList $ do
            (i, line) <- zip [0, 1 ..] m
            do
                (j, pos) <- zip [0, 1 ..] line
                guard $ pos == '#'
                return (j, i)
        cList = S.toList $ update 10 elves
        minX = minimum $ map fst cList
        maxX = maximum $ map fst cList
        minY = minimum $ map snd cList
        maxY = maximum $ map snd cList
        result = (maxX - minX + 1) * (maxY - minY + 1) - length cList
    putStrLn "Empty ground after 10 rounds:"
    putStrLn $ show result
    let rounds = run elves
    putStrLn "Total number of rounds:"
    putStrLn $ show rounds
    return (show result, show rounds)