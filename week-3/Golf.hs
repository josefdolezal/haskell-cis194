module Golf where

everyNth :: Int -> [a] -> [a]
everyNth n = map snd . filter (\(x,y) -> x `mod` n == 0) . zip [1..]

skips :: [a] -> [[a]]
skips = map (\(n,xs) -> everyNth n xs) . zip [1..] . clone
    where clone xs = replicate (length xs) xs

triplets :: [a] -> [(a,a,a)]
triplets (x:y:z:zs) = (x,y,z) : triplets (y : z : zs)
triplets _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(_,y,_) -> y) . filter (\(x,y,z) -> x < y && y > z) . triplets
