
-----------------------------------------------------------------------------------
-- Exercise 1. Wholemeal Programming
-----------------------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f
  where
    f :: Integer -> Integer
    f n = if even n then n `div` 2 else 3 * n + 1 


-----------------------------------------------------------------------------------
-- Exercise 2. Folding with Trees
-----------------------------------------------------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr buildTree Leaf

treeHeight :: Tree a -> Integer
treeHeight Leaf         = 0
treeHeight Node h l v r = h

buildTree :: a -> Tree a -> Tree a
buildTree n Leaf = Node 0 Leaf x Leaf
buildTree n (d l v r) =
  | 


