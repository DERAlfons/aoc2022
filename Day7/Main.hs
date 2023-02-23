module Day7.Main (main) where

import My.Parser (parserRegex, run)
import Data.Maybe (maybeToList)
import Data.Foldable (asum)

data Log = Cd String | Fn String Int | Up
data File = File String Int | Dir String [File]

parseDir :: [Log] -> String -> [File] -> (File, [Log])
parseDir [] dirName fileList = (Dir dirName fileList, [])
parseDir (Up : log) dirName fileList = (Dir dirName fileList, log)
parseDir (Fn fileName fileSize : log) dirName fileList = parseDir log dirName (File fileName fileSize : fileList)
parseDir (Cd dir : log) dirName fileList =
    let (file, rest) = parseDir log dir []
    in parseDir rest dirName (file : fileList)

parse :: [Log] -> File
parse (Cd rootDir : log) = fst $ parseDir log rootDir []

parseLine :: String -> Maybe Log
parseLine = run $ asum [
    parserRegex "^\\$ cd \\.\\.$" $ \[] -> Up,
    parserRegex "^\\$ cd (.*)$" $ \[dirName] -> Cd dirName,
    parserRegex "^([0-9]+) (.*)$" $ \[sz, fn] -> Fn fn (read sz)]

size :: File -> Int
size (File _ sz) = sz
size (Dir _ fs) = sum $ map size fs

dirs :: File -> [File]
dirs (File _ _) = []
dirs d @ (Dir _ fs) = d : (foldr (++) [] $ map dirs fs)

main :: IO (String, String)
main = do
    log <- (maybeToList . parseLine =<<) . lines <$> readFile "Day7/input.txt"
    let fileSystem = parse log
        dirSizes = map size $ dirs fileSystem
    putStrLn "Sum of sizes of small directories:"
    putStrLn $ show $ sum $ filter (<= 100000) $ dirSizes
    let delete = size fileSystem - 40000000
    putStrLn "Size of smallest directory, whose deletion frees up enough space:"
    putStrLn $ show $ minimum $ filter (>= delete) dirSizes
    return (show $ sum $ filter (<= 100000) $ dirSizes, show $ minimum $ filter (>= delete) dirSizes)