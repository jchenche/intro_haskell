{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 1
-- Just execute cabal list --installed and read up on it

-- Exercise 2
evalBattle :: [(Int, Int)] -> Battlefield -> Battlefield
evalBattle [] bf = bf
evalBattle ((attackerDV, defenderDV):battleDVs) (Battlefield attacker defender)
  | attackerDV > defenderDV = evalBattle battleDVs (Battlefield attacker (defender - 1))
  | otherwise               = evalBattle battleDVs (Battlefield (attacker - 1) defender)

reverseSort :: Ord a => [a] -> [a]
reverseSort = reverse . sort

zipBattleDieVals :: [DieValue] -> [DieValue] -> [(Int, Int)]
zipBattleDieVals attackerDVs defenderDVs = zip (reverseSort $ map unDV attackerDVs) (reverseSort $ map unDV defenderDVs)

maxAttackers :: Army -> Int
maxAttackers n = min 3 (n - 1)

maxDefenders :: Army -> Int
maxDefenders = min 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield attacker defender) = replicateM (maxAttackers attacker) die >>= \attackerDVs ->
                                            replicateM (maxDefenders defender) die >>= \defenderDVs ->
                                            return $ evalBattle (zipBattleDieVals attackerDVs defenderDVs) bf

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield 1 defender) = return bf
invade bf@(Battlefield attacker 0) = return bf
invade bf = battle bf >>= invade

-- Exercise 4
interpretWinner :: Battlefield -> Double
interpretWinner (Battlefield 1 defender) = 0
interpretWinner (Battlefield attacker 0) = 1

simulationRuns :: Int
simulationRuns = 1000

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM simulationRuns (invade bf) >>= \battleResults -> return $ (sum $ map interpretWinner battleResults) / fromIntegral simulationRuns

test = (evalRandIO $ successProb (Battlefield 11 10)) >>= print

-- Exercise 5
-- exactSuccessProb :: Battlefield -> Double
-- exactSuccessProb (Battlefield attacker defender) = 
