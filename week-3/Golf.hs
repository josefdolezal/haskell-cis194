module Golf where

import Data.Char

-- Exercise 1
everyNth :: Int -> [a] -> [a]
everyNth n = map snd . filter (\(x,y) -> x `mod` n == 0) . zip [1..]

skips :: [a] -> [[a]]
skips = map (\(n,xs) -> everyNth n xs) . zip [1..] . clone
    where clone xs = replicate (length xs) xs

-- Exercise 2
triplets :: [a] -> [(a,a,a)]
triplets (x:y:z:zs) = (x,y,z) : triplets (y : z : zs)
triplets _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(_,y,_) -> y) . filter (\(x,y,z) -> x < y && y > z) . triplets

-- Exercise 3
occurencies :: Eq a => a -> [a] -> Int
occurencies x = length . filter (\y -> y == x)

countElements :: [Integer] -> [Int]
countElements xs = map (\x -> occurencies x xs) [0..9]

graphLines :: [Int] -> [String]
graphLines xs = map (\x -> graphLine x xs) (reverse [1..(maximum xs)])

graphLine :: Int -> [Int] -> String
graphLine r xs = map (\x -> if x >= r then '*' else ' ') xs

histogram :: [Integer] -> String
histogram = unlines . (++ footer) . graphLines . countElements
    where footer = head (map (\xs -> [(replicate (length xs) '=')] ++ [xs]) [(map intToDigit [0..9])])
