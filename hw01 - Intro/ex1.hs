-- Verify credit card number

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x > 0     = x `mod` 10 : toDigitsRev (x `div` 10)
    | otherwise = []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper []       = []
doubleEveryOtherHelper (x:[])   = [x]
doubleEveryOtherHelper (x:y:xs) = x : y*2 : doubleEveryOtherHelper xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherHelper . reverse

sumDigit :: Integer -> Integer
sumDigit 0 = 0
sumDigit x = x `mod` 10 + sumDigit (x `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sumDigit x + sumDigits xs

checksum :: Integer -> Integer
checksum = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate x = checksum x `mod` 10 == 0


-- Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = [("illegal", "number")]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a
