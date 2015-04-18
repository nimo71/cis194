module Golf where

------------------------------------------------------------------------------
-- Exercise 1. Hopscotch
------------------------------------------------------------------------------

every :: Int -> ([a] -> [a])
every n = map snd . filter (\(x, _) -> (mod x n) == 0) . zip [1..]

skips :: [a] -> [[a]]
skips as = map (\(i, bs) -> every i bs) (zip [1..(length as)] (cycle [as]))


------------------------------------------------------------------------------
-- Exercise 2. Local Maxima
------------------------------------------------------------------------------

neighbours :: [a] -> [(a, a, a)]
neighbours xs = zip3 xs (drop 1 xs) (drop 2 xs)

withMaxima :: Ord a => [(a, a, a)] -> [(a, a, a)]
withMaxima = filter (\(x, y, z) -> (y > x && y > z))

extractMaxima :: [(a, a, a)] -> [a]
extractMaxima = map (\(_, y, _) -> y)

localMaxima :: [Integer] -> [Integer]
localMaxima = extractMaxima . withMaxima . neighbours


------------------------------------------------------------------------------
-- Exercise 3. Histogram
------------------------------------------------------------------------------

--
-- Count the number of times a digit occurs in a list
--
occurs :: [Integer] -> Integer -> Integer
occurs xs x = sum (map (\y -> if (y == x) then 1 else 0) xs)

--
-- For given a list of integers return a list of length 10 at each index of the list
-- store the number of occurences of the index value in the given list
-- 
digitsOccur :: [Integer] -> [Integer]
digitsOccur xs =  map (occurs xs) [0..9]

--
-- Decrement all the values in a given list by n if the result is < 0 store 0
-- 
decBy :: Integer -> [Integer] -> [Integer]
decBy n = map (max 0 . subtract n)

-- Given a list of values return a list whose first value is the given list and each
-- subsequent value a list where each value is decremented by one. The minimum value is
-- zero and any list containing all zero's is filtered out. 
-- 
reduceDown :: [Integer] -> [[Integer]]
reduceDown xs = filter (\ys -> (sum ys) > 0) (map (\n -> decBy n xs) [0..9])

--
-- Given a list of integers return a list of chars. Each char will be '*' or ' '
-- depending on whether its corresponding integer is > 0. '*' when greater than 0.
-- 
barline :: [Integer] -> String
barline xs = map (\x -> if x > 0 then '*' else ' ') xs

--
-- Given a list of integers count the occurences of each digit and build a histogram
-- based on the count of each occurence. The count will represent a column of '*' chars
-- at the index whose value is the same as the digit value.
--
histogramBars :: [Integer] -> [String]
histogramBars = reverse . map barline . reduceDown . digitsOccur

--
-- Build the footer for the histogram
-- 
histogramFooter :: [String]
histogramFooter = [take 10 (repeat '='), ['0'..'9']]

-- 
-- Append the footer strings to the histogram bar strings and call unline to convert
-- them in to a single string with newlines.
-- 
histogram :: [Integer] -> String
histogram xs = unlines ((histogramBars xs) ++ histogramFooter)
