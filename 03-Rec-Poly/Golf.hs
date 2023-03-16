module Golf where

-- EX 1 skips

takeNthElement :: Int -> [a] -> [a]
takeNthElement n = takeNthElementRec n 1

takeNthElementRec :: Int -> Int -> [a] -> [a]
takeNthElementRec _ _ [] = []
takeNthElementRec n c (x : xs)
  | c == n = x : takeNthElementRec n 1 xs -- take element
  | otherwise = takeNthElementRec n (c + 1) xs -- skip element

skips :: [a] -> [[a]]
skips [] = []
skips l = [takeNthElement i l | i <- [1 .. n]]
  where
    n = length l

-- EX 2 localMaxima

localMaxima :: [Integer] -> [Integer]
localMaxima (x1 : x2 : x3 : xs)
  | x1 < x2 && x2 > x3 = x2 : localMaxima ([x2, x3] ++ xs)
  | otherwise = localMaxima ([x2, x3] ++ xs)
localMaxima _ = []

-- EX 3 histogram

countOccurences :: [Integer] -> [Integer]
-- count occurrences of 0s to 9s
countOccurences = foldl incrementOccurence (take 10 [0, 0 .. 0])

incrementOccurence :: [Integer] -> Integer -> [Integer]
incrementOccurence res n = init start ++ [last start + 1] ++ end
  where
    start = take (fromIntegral n + 1) res -- last start is the one to increment
    end = drop (fromIntegral n + 1) res

decrementOrZero :: Integer -> Integer
decrementOrZero 0 = 0
decrementOrZero n = n - 1

spaceOrStar :: Integer -> Char
spaceOrStar 0 = ' '
spaceOrStar n = '*'

histogram :: [Integer] -> String
histogram l = histogramFromOccurrences (countOccurences l) ++ "==========\n0123456789"

histogramFromOccurrences :: [Integer] -> String
histogramFromOccurrences l
  | equalInitCounter l = ""
  | otherwise = histogramFromOccurrences (map decrementOrZero l) ++ map spaceOrStar l ++ "\n"

equalInitCounter :: [Integer] -> Bool
equalInitCounter l = l == take 10 [0, 0 .. 0]
