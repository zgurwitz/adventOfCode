import Data.Char ( isDigit )
import Data.List (elemIndex, tails, null)
import Data.Maybe ( isJust, fromJust )

myread :: String -> Int
myread s = 10 * first s + last s '0' where
    first (x:xs) | isDigit x = read [x]
                 | any ((isJust . numberToInt) . reverse) ((tails . reverse) $ takeWhile (not . isDigit) (x : xs)) = 
                    fromJust $ head $ filter isJust $ map (numberToInt.reverse) $ (tails . reverse) $ takeWhile (not . isDigit) (x:xs)
                 | otherwise = first xs
    first [] = undefined
    last [] n = read [n]
    last (x:xs) n | isDigit x = last xs x
                  | any ((isJust . numberToInt) . reverse) ((tails . reverse) $ takeWhile (not . isDigit) (x : xs)) = 
                    last xs $ head $ show $ fromJust $ head $ filter isJust $ map (numberToInt.reverse) $ (tails . reverse) $ takeWhile (not . isDigit) (x:xs)
                  | otherwise = last xs n  

numbers :: [String]
numbers = ["zero","one","two","three","four","five","six","seven","eight","nine"]

numberToInt :: String -> Maybe Int
numberToInt = flip elemIndex numbers

main = interact $ show . sum . map myread . words