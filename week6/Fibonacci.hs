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


-- More efficient
fib2 :: [Integer]
fib2 = 0 : 1 : zipWith (+) fib2 (tail fib2)


-- Streams

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer

