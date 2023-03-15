module Golf where

takeNthElement :: Int -> [a] -> [a]
takeNthElement 0 _ = []
takeNthElement n [] = []
takeNthElement n l = takeNthElementRec n 1 l

takeNthElementRec :: Int -> Int -> [a] -> [a]
takeNthElementRec 0 _ _ = []
takeNthElementRec _ 0 _ = []
-- first two should not happen
takeNthElementRec _ _ [] = []
takeNthElementRec n c (x : xs)
  | c == n = x : takeNthElementRec n 1 xs -- take element
  | otherwise = takeNthElementRec n (c + 1) xs -- skip element

skips :: [a] -> [[a]]
skips [] = []
skips l = skipsRec 1 l

skipsRec :: Int -> [a] -> [[a]]
skipsRec _ [] = []
skipsRec 0 _ = []
skipsRec n l
  | length l == n = [[last l]]
  | otherwise = takeNthElement n l : skipsRec (n + 1) l

localMaxima :: [Integer] -> [Integer]
localMaxima [x1, x2, x3] = localMaximaTest x1 x2 x3
localMaxima (x1 : x2 : x3 : xs) = localMaximaTest x1 x2 x3 ++ localMaxima ([x2, x3] ++ xs)

localMaximaTest :: Integer -> Integer -> Integer -> [Integer]
localMaximaTest a b c
  | a < b && b > c = [b]
  | otherwise = []

incremN :: [Integer] -> Integer -> [Integer]
-- input is a array of length 10 representing counters, and the one to increment
incremN [] _ = []
incremN (x : xs) 0 = (x + 1) : xs
incremN (x : xs) n = x : incremN xs (n - 1)

countOccurences :: [Integer] -> [Integer]
-- count occurrences of 0s to 9s
countOccurences l = countOccurencesRec l (take 10 [0, 0 .. 0])

countOccurencesRec :: [Integer] -> [Integer] -> [Integer]
countOccurencesRec [] res = res -- end of list
countOccurencesRec (x : xs) res = countOccurencesRec xs (init start ++ [last start + 1] ++ end)
  where
    start = take (fromIntegral x + 1) res -- last start is the one to increment
    end = drop (fromIntegral x + 1) res

decrementOrZero :: Integer -> Integer
decrementOrZero 0 = 0
decrementOrZero n = n - 1

spaceOrStar :: Integer -> Char
spaceOrStar 0 = ' '
spaceOrStar n = '*'

histogram :: [Integer] -> String
histogram l = histogramFromOccurrences (countOccurences l) ++ "==========\n0123456789"

histogramFromOccurrences :: [Integer] -> String
histogramFromOccurrences [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] = ""
histogramFromOccurrences l = histogramFromOccurrences (map decrementOrZero l) ++ map spaceOrStar l ++ "\n"
