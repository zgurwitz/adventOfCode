import Data.Char ( isDigit )

calculate :: [String] -> Int 
calculate as = calculate' 0 0 where
    calculate' x y | y >= length as = 0
                   | x >= length (head as) = calculate' 0 (y+1)
                   | otherwise = value x y as + calculate' (x + digits (readInt (drop x (as !! y)))) y

value :: Int -> Int -> [String] -> Int
value x y as = if touchesSymbol x y (digits v) as then v else 0 where
    v = readInt (drop x (as !! y)) 

touchesSymbol :: Int -> Int -> Int -> [String] -> Bool
touchesSymbol x y l as = any (\z -> not (isDigit z) && (z /= '.')) $ concatMap (take (l+2-isFirst x) . drop (x-1)) (take (3-isFirst y) $ drop (y-1) as) where
    isFirst n = if n == 0 then 1 else 0


readInt :: String -> Int
readInt xs | takeWhile isDigit xs == "" = 0
           | otherwise = read $ takeWhile isDigit xs

digits :: Int -> Int
digits x = digits' 1 x where
    digits' n x = if pow 10 n > x then n else digits' (n+1) x

pow :: Int -> Int -> Int
pow b 0 = 1
pow b n = b * pow b (n-1)

main :: IO ()
main = interact $ show . calculate . lines 