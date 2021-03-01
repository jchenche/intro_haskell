module Golf where

import Data.List

-- skips :: [a] -> [[a]]
-- skips [] = []
-- skips xs =
--     let
--         helper jumps []     = []
--         helper jumps (x:xs) = skip jumps 0 (x:xs) : helper (jumps+1) xs
--     in helper 1 xs

-- skip :: Int -> Int -> [a] -> [a]
-- skip jumps i [] = []
-- skip jumps i (x:xs)
--     | i `mod` jumps == 0 = x : skip jumps (i+1) xs
--     | otherwise          = skip jumps (i+1) xs


-- skips :: [a] -> [[a]]
-- skips l = map skip (zip (replicate (length l) l) [1..])

-- skip :: ([a], Int) -> [a]
-- skip (l, jump) = map fst (filter (\(_, idx) -> idx `mod` jump == 0) (zip l [1..]))


skips :: [a] -> [[a]]
skips l = map (\jump -> skip jump l) [1..length l]

skip :: Int -> [a] -> [a]
skip jump l = map fst (filter (\(_, idx) -> idx `mod` jump == 0) (zip l [1..]))


-- skips l = map (\jump -> (\jump l -> map fst (filter (\(_, idx) -> idx `mod` jump == 0) (zip l [1..]))) jump l) [1..length l]


localMaxima :: [Int] -> [Int]
localMaxima []                  = []
localMaxima (x:[])              = []
localMaxima (first:second:rest) =
    map (\(_, elem, _) -> elem) (filter (\(prev, elem, next) -> elem > prev && elem > next) (zip3 (first:second:rest) (second:rest) (rest)))


histogram :: [Int] -> String
histogram xs = intercalate "\n" (rotateCounterClockWise $ padding $ tally $ counts xs) ++ "\n==========\n0123456789\n"

count :: Int -> [Int] -> Int
count x = length . filter (==x)

counts :: [Int] -> [Int]
counts xs = map (flip count xs) [0..9]

tally :: [Int] -> [String]
tally = map (flip replicate '*')

padding :: [String] -> [String]
padding xs =
    let
        maxLength = length $ maximum xs
    in
        map (\elem -> take maxLength (elem ++ repeat ' ')) xs

rotateCounterClockWise :: [String] -> [String]
rotateCounterClockWise = reverse . transpose
