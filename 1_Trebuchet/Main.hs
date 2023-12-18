import Data.Char

myread :: String -> Int
myread s = 10 * first s + last s '0' where
    first (x:xs) | isDigit x = read [x]
                 | otherwise = first xs
    first [] = undefined
    last [] n = read [n]
    last (x:xs) n | isDigit x = last xs x
                  | otherwise = last xs n  


main = interact $ show . sum . map myread . words
