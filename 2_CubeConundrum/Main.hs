import Data.Char (isDigit)
games :: String -> [String]
games = splitOn "Game "

checkGame :: String -> Int
checkGame [] = 0
checkGame xs = if all possibleRound (splitOn "; " (drop 2 xs)) then read (takeWhile isDigit xs) else 0

possibleRound :: String -> Bool
possibleRound xs = all f (splitOn ", " xs) where
    f ys | (dropWhile isDigit ys !! 1) == 'r' = read (takeWhile isDigit ys) <= 12 
         | (dropWhile isDigit ys !! 1) == 'g' = read (takeWhile isDigit ys) <= 13
         | (dropWhile isDigit ys !! 1) == 'b' = read (takeWhile isDigit ys) <= 14
         | otherwise = True

takeWhile' :: ([a] -> Bool) -> [a] -> [a]
takeWhile' f (x:xs) = if f (x:xs) then x : takeWhile' f xs else []
takeWhile' _ _ = []

dropWhile' :: ([a] -> Bool) -> [a] -> [a]
dropWhile' f (x:xs) = if f (x:xs) then dropWhile' f xs else x:xs
dropWhile' _ _ = []

splitOn :: String -> String -> [String]
splitOn split (x:xs) = takeWhile' ((split /=) . take (length split)) (x:xs) : 
    splitOn split (drop (length split) (dropWhile' ((split /=) . take (length split)) (x:xs)))
splitOn _ _ = []

main = interact $ show . sum . map checkGame . games
