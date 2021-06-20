{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : (map last $ iterate (\fibs -> let l = length fibs in fibs ++ [fibs!!(l-1) + fibs!!(l-2)]) [0, 1])

-- Exercise 3
data Stream a = Cons' a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons' e s) = e : streamToList s

instance Show a => Show (Stream a) where
    show s = show (take 20 (streamToList s))

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons' x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons' e s) = Cons' (f e) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed getNewSeed seed = Cons' seed (streamFromSeed getNewSeed (getNewSeed seed))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons' e1 s1) s2 = Cons' e1 (interleaveStreams s2 s1)

largestPowerOfTwoThatDividesN :: (Integer, Integer) -> (Integer, Integer)
largestPowerOfTwoThatDividesN (prevN, _) =
    let n = prevN + 1
        exponents = [0..]
        powersLessThanN = reverse $ zip exponents $ takeWhile (<= n) $ iterate (2*) 1
    in (n, fst $ head $ filter (\(_, power) -> n `mod` power == 0) powersLessThanN)

-- ruler :: Stream Integer
-- ruler = streamMap snd $ streamFromSeed largestPowerOfTwoThatDividesN (1, 0)

ruler :: Stream Integer
-- ruler = interleaveStreams (streamRepeat 0) (interleaveStreams (streamRepeat 1) (interleaveStreams (streamRepeat 2) (interleaveStreams ...
-- ruler = interleaveStreams (streamRepeat 0) (streamMap (1+) ruler)
ruler =
    let
        helper :: Integer -> Stream Integer
        helper repeat = interleaveStreams (streamRepeat repeat) (helper (repeat+1))
    in helper 0

-- Exercise 6
x :: Stream Integer
x = Cons' 0 (Cons' 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons' n (streamRepeat 0)
    negate s = streamMap negate s
    (Cons' e1 s1) + (Cons' e2 s2) = Cons' (e1 + e2) (s1 + s2)
    (Cons' e1 s1) * b@(Cons' e2 s2) = Cons' (e1 * e2) (fromInteger e1 * s2 + s1 * b)

instance Fractional (Stream Integer) where
    a@(Cons' a0 a') / b@(Cons' b0 b') = Cons' (a0 `div` b0) ((a' - a / b * b') / fromInteger b0)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer
instance Num Matrix where
    (Matrix m00 m01 m10 m11) * (Matrix n00 n01 n10 n11) =
        Matrix (m00 * n00 + m01 * n10) (m00 * n01 + m01 * n11) (m10 * n00 + m11 * n10) (m10 * n01 + m11 * n11)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let (Matrix _ fn _ _) = (Matrix 1 1 1 0)^n in fn
