module Party where

import Employee
import Data.Tree
import Data.List

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons newEmployee (GL employees fun) = GL (newEmployee:employees) (empFun newEmployee + fun)

instance Semigroup GuestList where
    (GL emp1 fun1) <> (GL emp2 fun2) = GL (emp1 ++ emp2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2
    | compare g1 g2 == GT = g1
    | otherwise           = g2

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node rootLabel subForest) = f rootLabel (map (treeFold f) subForest)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss []             = (GL [boss] (empFun boss), GL [] 0)
nextLevel boss guestListPairs =
    let nextLevelWithBoss = foldr (<>) (GL [boss] (empFun boss)) (map snd guestListPairs)
        nextLevelWithoutBoss = mconcat (map (\(gl1, gl2) -> moreFun gl1 gl2) guestListPairs)
    in (nextLevelWithBoss, nextLevelWithoutBoss)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun hierarchy =
    let (gl1, gl2) = treeFold nextLevel hierarchy
    in moreFun gl1 gl2

-- Exercise 5
formatGuestList :: GuestList -> String
formatGuestList (GL employees fun) = "Total fun: " ++ show fun ++ "\n" ++ (unlines $ sort $ map empName employees)

main :: IO ()
main = readFile "company.txt" >>= (\contents -> putStrLn $ formatGuestList $ maxFun (read contents :: Tree Employee))
