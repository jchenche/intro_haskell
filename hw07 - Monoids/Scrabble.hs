{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0

score :: Char -> Score
score c
    | c' `elem` "aeioulnstr" = Score 1
    | c' `elem` "dg"         = Score 2
    | c' `elem` "bcmp"       = Score 3
    | c' `elem` "fhvwy"      = Score 4
    | c' `elem` "k"          = Score 5
    | c' `elem` "jx"         = Score 8
    | c' `elem` "qz"         = Score 10
    | otherwise              = Score 0
    where c' = toLower c

scoreString :: String -> Score
scoreString = mconcat . map score
