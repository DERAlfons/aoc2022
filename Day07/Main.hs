module Day07.Main (main) where

import Data.List (isPrefixOf)
import Data.Maybe (maybeToList)
import Data.Foldable (asum)

import My.Util (maybeToIO)
import My.Parser (Parser, parserRegex, parserList, run)

data File = File String Int | Dir String [File]

parseDir :: Parser File
parseDir = Dir <$>
    parserRegex "\\$ cd (.+)\\n" head <*>
    parserList "\\$ ls\\n" "" "(\\$ cd \\.\\.\\n)|$" (asum [
        parserRegex "(\\d+) (.+)\\n" $ \[sz, fn] -> File fn (read sz),
        parseDir])

size :: File -> Int
size (File _ sz) = sz
size (Dir _ fs) = sum $ map size fs

dirs :: File -> [File]
dirs (File _ _) = []
dirs d @ (Dir _ fs) = d : (dirs =<< fs)

main :: IO (String, String)
main = do
    log <- unlines . filter (not . ("dir" `isPrefixOf`)) . lines <$>
        readFile "Day07/input.txt"
    fileSystem <- maybeToIO "error parsing file system log" $ run parseDir log
    let dirSizes = map size $ dirs fileSystem

    let smallDirs = sum $ filter (<= 100000) dirSizes
    putStr "Sum of sizes of small directories: "
    print smallDirs

    let deleteSize = size fileSystem - 40000000
        deleteDir = minimum $ filter (>= deleteSize) dirSizes
    putStr "Size of smallest directory, where deletion frees up enough space: "
    print deleteDir

    return (show smallDirs, show deleteDir)