
----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

toDigitsRev' :: Integer -> [Integer] -> [Integer]
toDigitsRev' x xs = if x <= 0
                   then []
                   else if x < 10
                        then x : xs
                        else (mod x 10) : toDigitsRev' (div x 10) xs

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = toDigitsRev' x []

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [fst t * snd t | t <- zip (reverse xs) (cycle [1, 2])]


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sum ds | ds <- [toDigits x | x <- xs]]


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

validate :: Integer -> Bool
validate x = (mod (sumDigits (doubleEveryOther (toDigits x))) 10) == 0


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c = []
