{-# LANGUAGE LambdaCase #-}

import Data.Char ( isDigit )


data Atom = Gear | MT | N (Int, Int) deriving (Eq , Show)

isN (N _ ) = True
isN _ = False 

atomise :: [String] -> [[Atom]]
atomise = map f where
    f (y:ys) | isDigit y = zipWith (curry N) (replicate (digits ri) ri) [0..] ++ f (drop (digits ri) (y:ys)) 
             | y == '*' = Gear: f ys
             | otherwise = MT : f ys where ri = readInt (y:ys)
    f _ = []

gearRatios :: [String] -> Int
gearRatios xs = ratios 0 0 where
    ratios x y | y >= length as = 0
               | x >= length (head as) = ratios 0 (y+1)
               | otherwise = value x y as + ratios (x+1) y 
    as = atomise xs 

value :: Int -> Int -> [[Atom]] -> Int
value x y as | as !! y !! x == Gear = productA (concatMap ((:) MT . take (3-isFirst x) . drop (x-1)) (take (3-isFirst y) $ drop (y-1) as)) 
             | otherwise = 0 where
        isFirst n = if n == 0 then 1 else 0

productA :: [Atom] -> Int
productA xs = if length (split0 xs) == 2 then head (split0 xs) * (split0 xs !! 1) else 0 where
    split0 ((N (n,i)):ns) = n : split0 (dropWhile (\case
                                                    (N(f,s)) -> f == n
                                                    MT -> False
                                                    Gear -> False ) ns)  
    split0 (_:ns) = split0 ns
    split0 _ = []


readInt :: String -> Int
readInt xs | takeWhile isDigit xs == "" = 1
           | otherwise = read $ takeWhile isDigit xs

digits :: Int -> Int
digits x = digits' 1 x where
    digits' n x = if pow 10 n > x then n else digits' (n+1) x

pow :: Int -> Int -> Int
pow b 0 = 1
pow b n = b * pow b (n-1)

main :: IO ()
main = interact $ show . gearRatios . lines