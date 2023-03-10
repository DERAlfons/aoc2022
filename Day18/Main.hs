module Day18.Main (main) where

import Prelude hiding (map)
import Data.Set (
    Set, fromList, toList, insert,
    (\\), notMember, map, findMin, findMax)
import Data.Maybe (maybeToList)
import Control.Monad (guard)

import My.Parser (parserRegex, run)

type Cube = (Int, Int, Int)
type Cuboid = (Cube, Cube)

neighbours :: Cube -> [Cube]
neighbours (x, y, z) = [
    (x + 1, y, z), (x - 1, y, z),
    (x, y + 1, z), (x, y - 1, z),
    (x, y, z + 1), (x, y, z - 1)]

contains :: Cuboid -> Cube -> Bool
contains ((minX, minY, minZ), (maxX, maxY, maxZ)) (x, y, z) =
    minX <= x && x <= maxX &&
    minY <= y && y <= maxY &&
    minZ <= z && z <= maxZ

cuboidSurface :: Cuboid -> Int
cuboidSurface ((minX, minY, minZ), (maxX, maxY, maxZ)) =
    2 * (maxX - minX + 1) * (maxY - minY + 1) +
    2 * (maxX - minX + 1) * (maxZ - minZ + 1) +
    2 * (maxY - minY + 1) * (maxZ - minZ + 1)

getSurface :: Set Cube -> Int
getSurface cubes = length $ do
    cube <- toList cubes
    neighbour <- neighbours cube
    guard $ notMember neighbour cubes
    return ()

getCover :: Cuboid -> Set Cube -> Set Cube
getCover box cubes = loop cubes [fst box] \\ cubes
    where
    loop :: Set Cube -> [Cube] -> Set Cube
    loop cubes [] = cubes
    loop cubes (c : cs)
        | contains box c && notMember c cubes =
            loop (insert c cubes) (neighbours c ++ cs)
        | otherwise = loop cubes cs

parseCube :: String -> Maybe Cube
parseCube = run $ parserRegex "(\\d+),(\\d+),(\\d+)" $
    \[x, y, z] -> (read x, read y, read z)

main :: IO (String, String)
main = do
    cubes <- fromList <$> (maybeToList . parseCube =<<) <$>
        lines <$> readFile "Day18/input.txt"

    let surface = getSurface cubes
    putStr "Surface area: "
    print surface

    let minX = findMin (map (\(x, _, _) -> x) cubes) - 1
        maxX = findMax (map (\(x, _, _) -> x) cubes) + 1
        minY = findMin (map (\(_, y, _) -> y) cubes) - 1
        maxY = findMax (map (\(_, y, _) -> y) cubes) + 1
        minZ = findMin (map (\(_, _, z) -> z) cubes) - 1
        maxZ = findMax (map (\(_, _, z) -> z) cubes) + 1
        box = ((minX, minY, minZ), (maxX, maxY, maxZ))
        cover = getCover box cubes
        outerSurface = getSurface cover - cuboidSurface box
    putStr "Outer surface area: "
    print outerSurface

    return (show surface, show outerSurface)