
lists :: String -> ([Int], [Int])
lists xs = (winners, have) where
    info = tail $ tail $ dropWhile (/= ':') xs 
    winners = map read $ words $ takeWhile (/= '|') info
    have = map read $ words $ tail $ dropWhile (/= '|') info

changeNextN :: (Int -> Int) -> Int -> [Int] -> [Int]
changeNextN _ 0 xs = xs
changeNextN f i (x:xs) = f x: changeNextN f (i-1) xs
changeNextN _ _ _ = []

-- num cards | scores ref 
cards :: [Int] -> [Int] -> [Int]
cards _ [] = []
cards (n:ns) (s:ss) = n:cards (changeNextN (+n) s ns) ss 

matches :: [Int] -> [Int] -> Int
matches w = foldr (\x -> incrIfTrue (x `elem` w)) 0

incrIfTrue :: Bool -> Int -> Int
incrIfTrue b = if b then (+1) else id  

main :: IO()
main = interact $ show . sum . cards (repeat 1) . map (uncurry matches . lists) . lines