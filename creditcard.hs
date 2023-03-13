toDigits :: Integer ->  [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = split (show n)

split :: [Char] -> [Integer]
split s
    | s == "" = []
    | otherwise = [read [(head s)] :: Integer] ++ split (tail s)

toDigitsRev :: Integer ->  [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = nestedDoubleEveryOther l False

nestedDoubleEveryOther :: [Integer] -> Bool -> [Integer]
nestedDoubleEveryOther l b 
    | l == [] = []
    | b = nestedDoubleEveryOther (init l) (not b) ++ [(last l) * 2]
    | otherwise = nestedDoubleEveryOther (init l) (not b) ++ [(last l)]

sumDigits :: [Integer] -> Integer
sumDigits l = sumDigitsRec l 0

sumDigitsRec :: [Integer] -> Integer -> Integer
sumDigitsRec l s
    | l == [] = s
    | otherwise = s + sumDigitsRec (tail l) (sum (toDigits (head l)))

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0