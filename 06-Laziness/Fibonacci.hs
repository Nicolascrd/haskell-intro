{-# LANGUAGE FlexibleInstances #-}
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
