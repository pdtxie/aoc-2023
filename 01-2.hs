import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    input <- readFile "01.txt"
    print $ sum $ map getFirstLast $ words $ input

getDigit :: [Char] -> Maybe Char
getDigit [] = Nothing
getDigit s@(x : _)
    | isPrefixOf "one" s    = Just '1'
    | isPrefixOf "two" s    = Just '2'
    | isPrefixOf "three" s  = Just '3'
    | isPrefixOf "four" s   = Just '4'
    | isPrefixOf "five" s   = Just '5'
    | isPrefixOf "six" s    = Just '6'
    | isPrefixOf "seven" s  = Just '7'
    | isPrefixOf "eight" s  = Just '8'
    | isPrefixOf "nine" s   = Just '9'
    | isDigit x             = Just x
    | otherwise             = Nothing

getFirstLast :: [Char] -> Int
getFirstLast x = 10 * (digitToInt $ head nums) + (digitToInt $ last nums)
    where nums = mapMaybe getDigit $ tails x


