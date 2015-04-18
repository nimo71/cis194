{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, InstanceSigs #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score s) = s

scoreString :: String -> Score
scoreString = foldl (+) 0 . map score

class Scored a where
  score :: a -> Score

instance Scored Char where
  score :: Char -> Score
  score c
    | elem lc "aeilnorstu" = Score 1
    | elem lc "dg"         = Score 2
    | elem lc "bcmp"       = Score 3
    | elem lc "fhvwy"      = Score 4
    | lc ==   'k'          = Score 5
    | elem lc "jx"         = Score 8
    | elem lc "qz"         = Score 10
    | otherwise            = Score 0
    where lc = toLower c                       

instance Scored Score where
  score = id

instance Scored a => Scored (a, b) where
  score = score . fst

instance Monoid Score where
  mempty = Score 0
  mappend = (+)
