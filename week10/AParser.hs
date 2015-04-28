{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail


-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)


{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (res, str) = (f res, str)


{-
To create a Functor on Parser fmap takes a function, g :: a -> b and, parser Parser a
The resulting parser is created by mapping the function (first g) over the result of
runParser parser...

  g                :: a -> b
  parser           :: Parser a
  runParser        :: Parser a -> String -> Maybe (a, String)
  runParser parser :: String -> Maybe (a, String)
  first g          :: (a, String) -> (b, String)
  (first g) `fmap` (runParser parser) :: String -> Maybe (b, String)

-}

instance Functor Parser where
  fmap f parser = Parser parse
    where parse s = first f <$> runParser parser s


------------------------------------------------------------------------------------
-- Exercise 2
------------------------------------------------------------------------------------
-- Applicative instance for Parser
--
-- pure a represents an instance that consumes input and successfuly returns a result
-- of a
--   pure :: (String -> a) -> Parser (String -> (a, string))
--
-- p1 <*> p2 represents the parser which first runs p1 (which will consume some input
-- and produce a function), then passes the remaining input to p2 (which consumes more
-- input and produces some value. However if p1 and p2 fails then the whole thing
-- should also fail (put another way, p1 <*> p2 succeeds if both p1 an p2 succeed).
--
-- runParser            :: Parser a -> String -> Maybe (a, String)
-- runParser (Parser a) :: String -> Maybe (a, String) 

instance Applicative Parser where
  pure parse = Parser (\s -> Just (parse, s))

  p1 <*> p2 = Parser (\s ->
                case runParser p1 s of
                  Nothing           -> Nothing
                  Just (res1, rem1) -> first res1 <$> runParser p2 rem1)
{-
  p1 <*> p2 = Parser parse
                where parse s = do
                        (res1, rem1) <- runParser p1 s
                        (res2, rem2) <- runParser p2 rem1
                        return (res1 $ res2, rem2)
-}


------------------------------------------------------------------------------------
-- Exercise 3
------------------------------------------------------------------------------------

abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\i _ j-> [i, j]) <$> posInt <*> char ' ' <*> posInt 


------------------------------------------------------------------------------------
-- Exercise 4
------------------------------------------------------------------------------------
{-
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

Alternative instance for Parser where:

empty is a parser that always fails

p1 <|> p2 represents a parser that first tries running p1. If p1 succeeds then p2 is
ignored and the result of p1 is returned. Otherwise, if p1 fails, then p2 is tried
instead.
-}

instance Alternative Parser where
  empty = Parser $ const Nothing

  p1 <|> p2 = Parser (\s ->
                case runParser p1 s of
                  Nothing -> runParser p2 s
                  res1 -> res1) 


------------------------------------------------------------------------------------
-- Exercise 5
------------------------------------------------------------------------------------

intOrUpperCase :: Parser ()
intOrUpperCase = const () <$> posInt <|> const () <$> satisfy (isUpper)
