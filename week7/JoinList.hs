{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, InstanceSigs #-}
module JoinList where

import Buffer
import Data.Monoid
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

sz :: (Sized b, Monoid b) => JoinList b a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ i _ | i < 0      = Nothing
indexJ i jl | i > sz jl = Nothing
indexJ _ (Single _ v)   = Just v
indexJ n (Append _ l r)
  | (n < sz l)          = indexJ n l
  | otherwise           = indexJ (n - sz l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty          = Empty
dropJ n jl | n <= 0    = jl 
dropJ _ (Single _ _)   = Empty
dropJ n (Append _ l r)
  | n < sz l           = (dropJ n l) +++ r
  | otherwise          = dropJ (n - sz l) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty              = Empty
takeJ n _ | n < 0          = Empty
takeJ _ sjl @ (Single _ _) = sjl
takeJ n (Append _ l r)
  | n < sz l               = takeJ n l
  | otherwise              = l +++ takeJ (n - sz l) r
                             
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreSizeLine :: String -> (JoinList (Score, Size) String)
scoreSizeLine l = Single (scoreString l, Size 1) l;

instance Buffer (JoinList (Score, Size) String) where

  toString :: JoinList (Score, Size) String -> String
  toString Empty          = ""
  toString (Single _ s)   = s
  toString (Append _ l r) = toString l ++ toString r

  fromString :: String -> JoinList (Score, Size) String
  fromString = foldl (+++) Empty . map scoreSizeLine . lines

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ
  
  replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
  replaceLine i l jl = takeJ i jl +++ scoreSizeLine l +++ dropJ (i + 1) jl
    
  numLines :: JoinList (Score, Size) String -> Int
  numLines = getSize . size .tag

  value :: JoinList (Score, Size) String -> Int
  value = getScore . score . tag

main :: IO()
main = runEditor editor jl
  where jl = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String
