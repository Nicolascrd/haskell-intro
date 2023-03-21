{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- EX 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- EX 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- EX 3

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

-- EX 4

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) = Stream (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-- EX 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamMap (largestPowerOf2Dividing . (+ 1)) nats

largestPowerOf2Dividing :: Integer -> Integer
largestPowerOf2Dividing 0 = 0
largestPowerOf2Dividing n
  | even n = 1 + largestPowerOf2Dividing (n `div` 2)
  | otherwise = 0

-- EX 5 second approach

-- interleaveStreams :: Stream a -> Stream a -> Stream a
-- interleaveStreams (Stream x s) (Stream x' s') = Stream x (Stream x' (interleaveStreams s s'))
-- does not work because must evaluate both streams

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x s) s' = Stream x (interleaveStreams s' s)

newRuler :: Stream Integer
newRuler = foldr1 interleaveStreams (streamToList (streamMap streamRepeat nats))

-- EX 6

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0)
  negate (Stream n s) = Stream (-n) (negate s)
  (+) (Stream n s) (Stream n' s') = Stream (n + n') (s + s')
  (*) (Stream n s) (Stream n' s') = Stream (n * n') (streamMap (* n) s' + (s * Stream n' s'))

instance Fractional (Stream Integer) where
  (/) (Stream n s) (Stream n' s') = Stream (n `div` n') (streamMap (`div` n') (s - ((Stream n s / Stream n' s') * s')))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- EX 7

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix a' b' c' d') = Matrix (a * a' + b * c') (a * b' + b * d') (c * a' + d * c') (c * b' + d * d')

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = fibFromMatrix (Matrix 1 1 1 0 ^ n)
  where
    fibFromMatrix (Matrix a b c d) = b