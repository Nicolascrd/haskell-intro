type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3
  | n == 0 = []
  | otherwise = hanoi (n - 1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (n - 1) p3 p2 p1

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]