{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- Exercise 2
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Examples of join lists:
-- Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')
-- Append (Size 5) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Append (Size 2) (Single (Size 1) 'h') (Single (Size 1) 'u'))
-- Append (Size 5) (Append (Size 2) (Single (Size 1) 'h') (Single (Size 1) 'u')) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a')))

-- Test: (map (jlToList jl !!?) [-2..6]) == (map (flip indexJ jl) [-2..6])
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty        = Nothing
indexJ i _ | i < 0    = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ jl1 jl2)
    | i < leftSize    = indexJ i jl1
    | otherwise       = indexJ (i - leftSize) jl2
    where leftSize = getSize $ size $ tag jl1

-- Test: (map (flip drop (jlToList jl)) [-2..6]) == (map (jlToList . flip dropJ jl) [-2..6])
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0        = jl
dropJ n Empty              = Empty
dropJ n (Single _ _)       = Empty
dropJ n (Append _ jl1 jl2) = (dropJ n jl1) +++ (dropJ (n - leftSize) jl2)
    where leftSize = getSize $ size $ tag jl1

-- Test: (map (flip take (jlToList jl)) [-2..6]) == (map (jlToList . flip takeJ jl) [-2..6])
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0         = Empty
takeJ n Empty              = Empty
takeJ n (Single m a)       = Single m a
takeJ n (Append _ jl1 jl2) = (takeJ n jl1) +++ (takeJ (n - leftSize) jl2)
    where leftSize = getSize $ size $ tag jl1

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
    toString Empty              = ""
    toString (Single _ a)       = a
    toString (Append _ jl1 jl2) = toString jl1 ++ toString jl2

    fromString = foldr (+++) Empty . map (\line -> Single (scoreString line, Size 1) line) . lines

    line = indexJ

    replaceLine n newContent buffer = takeJ n buffer +++ fromString newContent +++ dropJ (n+1) buffer

    numLines = getSize . size . tag

    value = getScore . fst . tag

greeting :: String
greeting = unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor $ initBuffer
    where initBuffer = fromString greeting :: JoinList (Score, Size) String
