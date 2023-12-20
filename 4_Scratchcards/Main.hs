
lists :: String -> ([Int], [Int])
lists xs = (winners, have) where
    info = tail $ tail $ dropWhile (/= ':') xs 
    winners = map read $ words $ takeWhile (/= '|') info
    have = map read $ words $ tail $ dropWhile (/= '|') info

matches :: [Int] -> [Int] -> Int
matches w = foldr (\x -> incrIfTrue (x `elem` w)) 0

incrIfTrue :: Bool -> Int -> Int
incrIfTrue b = if b then (+1) else id  

scoreConvert :: Int -> Int
scoreConvert x | x < 1 = 0
               | x == 1 = 1
               | otherwise = 2 * scoreConvert (x-1)

main :: IO()
main = interact $ show . sum .  map (scoreConvert . uncurry matches . lists) . lines