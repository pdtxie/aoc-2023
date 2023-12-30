import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Data.List (sortBy, groupBy, maximumBy)
import Data.Function (on)

main :: IO ()
main = do
    input <- readFile "02.txt"
    print $ solve $ lines input

parse :: String -> [String]
parse xs = tail $ tail split
    where split = words $ filter (\x -> x /= ',' && x /= ';' &&  x /= ':') xs
          num   = read (split !! 1) :: Int

getMax :: [String] -> Int
getMax xs = product
            $ map (head . maximumBy (compare `on` head))
            $ groupBy ((==) `on` last)
            $ sortBy (compare `on` last)
            $ chunksOf 2
            $ map ((read :: String -> Int) . replace) xs

solve :: [String] -> Int
solve lines = sum $ map (getMax . parse) lines

replace :: String -> String
replace "green" = "0"
replace "red" = "1"
replace "blue" = "2"
replace x = x
