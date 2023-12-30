import Data.Char (digitToInt)
import Data.List.Split (splitOn, chunksOf)
import Data.List (transpose)

main :: IO ()
main = do
    input <- readFile "02.txt"
    print $ sum $ map solve $ lines input

solve :: String -> Int
solve x = if (all countBalls removed) then num else 0
    where split = map words $ splitOn ";" $ filter (/= ',') x
          num   = read $ init (head split !! 1) :: Int
          removed = (drop 2 $ head split) : (tail split)

countBalls :: [String] -> Bool
countBalls x = (length $ filter (\x -> x) $ map (uncurry (>)) paired) == 0
    where skip = transpose $ chunksOf 2 x
          paired = zip (map read (skip !! 0) :: [Int]) (map replace (skip !! 1))

replace :: [Char] -> Int
replace "green" = 13
replace "red" = 12
replace "blue" = 14
