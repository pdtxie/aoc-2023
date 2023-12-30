import Data.Char

main :: IO ()
main = do
    input <- readFile "01.txt"
    print . sum $ map getFirstLast $ words $ input

getFirstLast :: [Char] -> Int
getFirstLast x = 10 * (digitToInt a) + (digitToInt b)
    where filtered = filter isDigit x
          a        = head filtered
          b        = last filtered
