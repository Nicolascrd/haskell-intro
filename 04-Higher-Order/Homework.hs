{-# OPTIONS_GHC -Wall #-}

-- EX 1

fun1 :: [Integer] -> Integer
fun1 = foldl1 (\x y -> y * if even x then x - 2 else x)

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- EX 2

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

getTreeHeight :: Tree a -> Integer
getTreeHeight Leaf = -1
getTreeHeight (Node _ left _ right) = 1 + max (getTreeHeight left) (getTreeHeight right)

getShortestBranch :: Tree a -> Integer
getShortestBranch Leaf = -1
getShortestBranch (Node _ left _ right) = 1 + min (getShortestBranch left) (getShortestBranch right)

addNodeToTree :: Tree a -> a -> Tree a
addNodeToTree Leaf newNode = Node 0 Leaf newNode Leaf
addNodeToTree (Node height left val right) newNode =
  if addToLeft
    then Node height (addNodeToTree left newNode) val right
    else Node height left val (addNodeToTree right newNode)
  where
    addToLeft = getShortestBranch left < getShortestBranch right

recomputeHeights :: Tree a -> Tree a
recomputeHeights Leaf = Leaf
recomputeHeights (Node h left val right) = Node (getTreeHeight (Node h left val right)) (recomputeHeights left) val (recomputeHeights right)

addNodeToTreeAndRecompute :: Tree a -> a -> Tree a
addNodeToTreeAndRecompute t val = recomputeHeights $ addNodeToTree t val

foldTree :: [a] -> Tree a
foldTree = foldl addNodeToTreeAndRecompute Leaf

-- EX 3

xor :: [Bool] -> Bool
xor l = odd (foldl incrementIfTrue 0 l)

incrementIfTrue :: Integer -> Bool -> Integer
incrementIfTrue i b
  | b = i + 1
  | otherwise = i

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> f x : l) []

-- EX 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> (x * 2) + 1) (filter ( `notElem` toRemove) [1 .. n])
  where
    toRemove = generateRemoved n

generateRemoved :: Integer -> [Integer]
-- i + j + 2ij <= n
generateRemoved n = filter (>0) $ filter (<= n) (map (\(x, y) -> if x > y then 0 else x + y + 2 * x * y) (cartProd [1 .. n] [1 .. n]))

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]