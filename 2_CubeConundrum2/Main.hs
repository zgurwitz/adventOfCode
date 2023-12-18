import Data.Char (isDigit)

games :: String -> [String]
games xs = map (dropWhile (/= ':')) $ splitOn "Game " xs

takeWhile' :: ([a] -> Bool) -> [a] -> [a]
takeWhile' f (x:xs) = if f (x:xs) then x : takeWhile' f xs else []
takeWhile' _ _ = []

dropWhile' :: ([a] -> Bool) -> [a] -> [a]
dropWhile' f (x:xs) = if f (x:xs) then dropWhile' f xs else x:xs
dropWhile' _ _ = []

gameMax :: String -> (Int,Int,Int)
gameMax xs = max' $ map roundMax (splitOn "; " (drop 2 xs))

roundMax :: String -> (Int, Int, Int)
roundMax xs = sum' (map f (splitOn ", " xs)) where
    f ys | (dropWhile isDigit ys !! 1) == 'r' = (read (takeWhile isDigit ys),0,0)  
         | (dropWhile isDigit ys !! 1) == 'g' = (0,read (takeWhile isDigit ys),0)
         | (dropWhile isDigit ys !! 1) == 'b' = (0,0,read (takeWhile isDigit ys))
         | otherwise = (0,0,0)

power' :: (Int,Int,Int) -> Int
power' (a,b,c) = a*b*c

sum' :: [(Int,Int,Int)] -> (Int,Int,Int)
sum' = foldr (\(a,b,c) (x,y,z) -> (a+x,b+y,c+z)) (0,0,0)

max' :: [(Int,Int,Int)] -> (Int,Int,Int)
max' = foldr (\(a,b,c) (x,y,z) -> (max a x,max b y,max c z)) (0,0,0)


splitOn:: String -> String -> [String]
splitOn split (x:xs) = takeWhile' ((split /=) . take (length split)) (x:xs) : 
    splitOn split (drop (length split) (dropWhile' ((split /=) . take (length split)) (x:xs)))
splitOn _ _ = []

main = interact $ show . sum . map (power' . gameMax) . games