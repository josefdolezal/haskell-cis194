toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigits (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x
    | length x `mod` 2 == 0 = zipWith (*) x (cycle [2,1])
    | otherwise = zipWith (*) x (cycle [1,2])

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

validate :: Integer -> Bool
validate n
    | n <= 0 = False
    | otherwise = sumDigits (doubleEveryOther (toDigits n)) `mod` 2 == 0
