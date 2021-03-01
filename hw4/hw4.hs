-- Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even
            . takeWhile (>1)
            . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


-- Folding with trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert elem Leaf                          = Node 0 Leaf elem Leaf
insert elem (Node rootH Leaf rootN Leaf)  = Node 1 (insert elem Leaf) rootN Leaf
insert elem (Node rootH Leaf rootN right) = Node rootH (insert elem Leaf) rootN right
insert elem (Node rootH left rootN Leaf)  = Node rootH left rootN (insert elem Leaf)
insert elem (Node rootH left@(Node leftH _ _ _) rootN right@(Node rightH _ _ _))
    | leftH < rightH  = let newLeft@(Node newLeftH _ _ _) = (insert elem left) in Node (max (newLeftH + 1) rootH) newLeft rootN right
    | otherwise       = let newRight@(Node newRightH _ _ _) = (insert elem right) in Node (max rootH (newRightH + 1)) left rootN newRight

-- Node 3
--     (Node 2
--         (Node 0 Leaf 'F' Leaf)
--         'I'
--         (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
--     'J'
--     (Node 2
--         (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
--         'H'
--         (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))


-- More folds!

xor :: [Bool] -> Bool
xor = foldr cancelTrues False

cancelTrues :: Bool -> Bool -> Bool
cancelTrues True False = True
cancelTrues False True = True
cancelTrues _ _        = False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\elem acc -> f elem : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\)


-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn