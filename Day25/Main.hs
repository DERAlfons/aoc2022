module Day25.Main (main) where

value :: Char -> Int
value '=' = -2
value '-' = -1
value '0' = 0
value '1' = 1
value '2' = 2

symbol :: Int -> Char
symbol (-2) = '='
symbol (-1) = '-'
symbol 0 = '0'
symbol 1 = '1'
symbol 2 = '2'

stoi :: String -> Int
stoi s = sum $ zipWith (*) (map (5 ^) [0, 1 ..]) (map value $ reverse s)

ito5 :: Int -> [Int]
ito5 0 = []
ito5 n = (n `mod` 5) : (ito5 (n `div` 5))

_5tos :: [Int] -> String
_5tos ds = map symbol . reverse $ loop ds 0
    where
    loop [] 0 = []
    loop [] 1 = [1]
    loop (d : ds) c =
        let dn = ((d + c + 2) `mod` 5) - 2
        in if d + c >= 5 || dn < 0 then
               dn : (loop ds 1)
           else
               dn : (loop ds 0)

main :: IO (String, String)
main = do
    result <- _5tos . ito5 . sum . map stoi . lines <$> readFile "Day25/input.txt"
    putStrLn "Total fuel:"
    putStrLn result
    return (result, "No second task :)")