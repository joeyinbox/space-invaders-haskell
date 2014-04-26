module Attacker where

import Data.Map
import Utils
import Bullet


-- This list store the identifier of all attackers
attackerIdList = [1..55]

-- Define Attacker types
data AttackerType = Crab | Octopus | Squid | Spaceship

-- Define Attacker type checking
attackerTypeEq :: AttackerType -> AttackerType -> Bool
attackerTypeEq Crab      Crab      = True
attackerTypeEq Octopus   Octopus   = True
attackerTypeEq Squid     Squid     = True
attackerTypeEq Spaceship Spaceship = True
attackerTypeEq _         _         = False


-- Return the worth value of an Attacker type
-- For now, the value of the spaceship is fixed because of IO limitations (use of MonadRandom package?)
getAttackerWorth :: AttackerType -> Int
getAttackerWorth t = do
    if attackerTypeEq Crab t 
        then 20
    else if attackerTypeEq Octopus t 
        then 30
    else if attackerTypeEq Squid t 
        then 10
    else if attackerTypeEq Spaceship t 
        then 200
    else 0


-- Return a list of all alive attackers
getAliveAttackerList :: [(Int, Bool)] -> [Int]
getAliveAttackerList [] = []
getAliveAttackerList (x:xs) = do
    if snd x
        then fst x : getAliveAttackerList xs
    else getAliveAttackerList xs

-- Return a list of tuples (Int, Position) of all alive attackers
getAliveAttackerPositionList :: [(Int, Bool)] -> [(Int, Position)] -> [(Int, Position)]
getAliveAttackerPositionList [] _ = []
getAliveAttackerPositionList (x:xs) yys = do
    if snd x
        then (fst x,(fromList yys ! fst x)) : getAliveAttackerPositionList xs yys
    else getAliveAttackerPositionList xs yys


-- (re-)Initialise the list of attacker types
resetAttackerTypeList :: [(Int, AttackerType)]
resetAttackerTypeList = merge ([1..11] `zip` (cycle [Squid])) (merge ([12..33] `zip` (cycle [Crab])) ([34..55] `zip` (cycle [Octopus])))

-- (re-)Initialise the list indicating if the attackers are still alive
resetAttackerAliveList :: [(Int, Bool)]
resetAttackerAliveList = attackerIdList `zip` (cycle [True])

-- (re-)Initialise the list of attackers position
resetAttackerPositionList :: [(Int, Position)]
resetAttackerPositionList = do
    -- Horizontal positions
    let x = (Prelude.map (+90) (Prelude.map (*60) [1..11])) ++ (Prelude.map (+85) (take 44 (cycle (Prelude.map (*60) [1..11]))))

    -- Vertical positions
    let y = (take 11 [160,160..]) ++ (take 11 [220,220..]) ++ (take 11 [280,280..]) ++ (take 11 [340,340..]) ++ (take 11 [400,400..])

    -- Combine both x and y then map it to all identifiers
    attackerIdList `zip` (addPosition x y)

-- (re-)Initialise attackers direction
resetAttackerDirection :: Int
resetAttackerDirection = 1

-- Shift all X positions by a given number
moveAttackerSide :: [(Int, Position)] -> Int -> [(Int, Position)]
moveAttackerSide xs z = Prelude.map (\(x,y) -> (x, (fst y+z, snd y))) xs

-- Shift all Y positions by a given number
moveAttackerDown :: [(Int, Position)] -> Int -> [(Int, Position)]
moveAttackerDown xs z = Prelude.map (\(x,y) -> (x, (fst y, snd y+z))) xs


-- Get the position of the lowest attackers per column
getLowestAttackerPositionList :: [(Int, Position)] -> [(Int, Position)]
getLowestAttackerPositionList xs = go [1..11] xs
    where
        go []     _   = []
        go (y:ys) xxs = do
            let list = getAliveAttackerPositionListFromIdentifierList (y : (Prelude.map (+y) (Prelude.map (*11) [1..4]))) xxs
            
            if length list == 0
                then go ys xxs
            else getLowestAttackerListPerColumn list ++ go ys xxs


-- Get the position of the lowest attackers per column
getLowestAttackerListPerColumn :: [(Int, Position)] -> [(Int, Position)]
getLowestAttackerListPerColumn []     = []
getLowestAttackerListPerColumn (x:xs) = go x xs
    where
        go m []     = [m]
        go m (y:ys) = if snd (snd m) > snd (snd y) then go m ys else go y ys




getAliveAttackerPositionListFromIdentifierList :: [Int] -> [(Int, Position)] -> [(Int, Position)]
getAliveAttackerPositionListFromIdentifierList []     _   = []
getAliveAttackerPositionListFromIdentifierList (x:xs) yys = do
    if isInList x yys
        then (x, (fromList yys ! x)) : getAliveAttackerPositionListFromIdentifierList xs yys
    else getAliveAttackerPositionListFromIdentifierList xs yys



makeAttackerShoot :: [(Int, Position)] -> [Bullet] -> Int -> Int -> [Bullet]
makeAttackerShoot x y r t = go x y [1..((r `rem` 5)+(2*(1+t)))] r t
    where
        go [] _ _      _ _ = y
        go _  y []     _ _ = y
        go x  y (z:zs) r t = go x (addBullet y ((fst (snd (x !! (rem r (length x)))))+19, (snd (snd (x !! (rem r (length x)))))+36) 1 False) zs r t



















