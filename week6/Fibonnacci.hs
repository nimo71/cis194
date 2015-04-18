module Fibonacci where

--
-- The Fibonacci numbers Fn are defined as the sequence of integers, beginning with 0 and 1, where every integer in the sequence is the sum of the previous two. That is,
--   F0 = 0
--   F1 = 1
--   Fn = Fn−1 + Fn−2 (n ≥ 2)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- The infinate fibonacci series

fib1 :: [Integer]
fib1 = map fib [0..]


