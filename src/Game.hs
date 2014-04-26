module Game where

import Graphics.UI.SDL
import Data.Map
import Dataset
import Attacker
import Bullet
import Utils



-- Return the general left boundary of all attackers
getLeftBoundary [] = error "The list is empty"
getLeftBoundary (x:xs) = go (fst (snd x)) xs
    where
        go m [] = m 
        go m (y:ys) = if m < (fst (snd y)) then go m ys else go (fst (snd y)) ys

-- Return the left boundary for a specified line
getLeftBoundaryPerLine [] _ = error "The list is empty"
getLeftBoundaryPerLine (x:xs) line = go (fst (snd x)) (reverse (take 11 (reverse (take (line*11) xs))))
    where
        go m [] = m 
        go m (y:ys) = if m < (fst (snd y)) then go m ys else go (fst (snd y)) ys
    
getRightBoundary [] = error "The list is empty"
getRightBoundary (x:xs) = go (fst (snd x)) xs
    where
        go m [] = m 
        go m (y:ys) = if m > (fst (snd y)) then go m ys else go (fst (snd y)) ys

getRightBoundaryPerLine [] _ = error "The list is empty"
getRightBoundaryPerLine (x:xs) line = go (fst (snd x)) (reverse (take 11 (reverse (take (line*11) xs))))
    where
        go m [] = m 
        go m (y:ys) = if m > (fst (snd y)) then go m ys else go (fst (snd y)) ys

getDownBoundary [] = error "The list is empty"
getDownBoundary (x:xs) = go (snd (snd x)) xs
    where
        go m [] = m 
        go m (y:ys) = if m > (snd (snd y)) then go m ys else go (snd (snd y)) ys

-- Detect if attackers need to turn
detectTurn :: [(Int, Position)] -> Int -> Bool
detectTurn positionList direction = do
    if direction == -1 && (getLeftBoundary positionList) == 1
        then True
    else if direction == 1 && (getRightBoundary positionList) == 976
        then True
    else False

-- Detect if attackers have just landed
detectGameOver :: [(Int, Position)] -> Bool
detectGameOver positionList = do
    if getDownBoundary positionList == 672
        then True
    else False




-- Mark all attackers which enter in collision with a bullet as not alive
--keepUntouchedAttackerList :: [(Int, Bool)] -> [(Position, Int)] -> GameDataType -> AppDataType -> [(Int, Bool)]
keepUntouchedAttackerList []     _   _ _ = []
keepUntouchedAttackerList (x:xs) yys g a = do
    if not (snd x)
        then x : keepUntouchedAttackerList xs yys g a
    else do
        -- Determine the attacker size
        let size = if attackerTypeEq (fromList (getAttackerTypeList g) ! (fst x)) Crab
                        then (surfaceGetWidth (getCrabImg a), surfaceGetHeight (getCrabImg a))
                    else if attackerTypeEq (fromList (getAttackerTypeList g) ! (fst x)) Octopus
                        then (surfaceGetWidth (getOctopusImg a), surfaceGetHeight (getOctopusImg a))
                    else if attackerTypeEq (fromList (getAttackerTypeList g) ! (fst x)) Squid
                        then (surfaceGetWidth (getSquidImg a), surfaceGetHeight (getSquidImg a))
                    else (surfaceGetWidth (getSpaceshipImg a), surfaceGetHeight (getSpaceshipImg a))
        
        -- Determine the attacker position
        let pos = (fromList (getAttackerPositionList g) ! (fst x))
    
        -- Check if the current attacker touch any of all the bullets
        detectTouchedAttacker x yys size pos : keepUntouchedAttackerList xs yys g a



-- Check if an attacker touch any of all the bullets
detectTouchedAttacker :: (Int, Bool) -> [(Position, Int)] -> (Int, Int) -> Position -> (Int, Bool)
detectTouchedAttacker x  []     _    _ = x
detectTouchedAttacker x  (y:ys) size pos = do
    -- Check if the current bullet position asserted corresponds to the position of the current attacker
    --    bullet X   >= attacker X&&    bullet X   <=   attacker X + width   &&    bullet Y   >= attacker Y&&    bullet Y   <=  attacker Y + height
    if (fst (fst y)) >= (fst pos) && (fst (fst y)) <= ((fst pos)+(fst size)) && (snd (fst y)) >= (snd pos) && (snd (fst y)) <= ((snd pos)+(snd size))
        then (fst x, False)
    else detectTouchedAttacker x ys size pos





-- Return the list of points available in a list of attackers
getEarnedPoints :: [(Int, Bool)] -> [(Int, AttackerType)] -> [Int]
getEarnedPoints []     _  = []
getEarnedPoints (x:xs) ys = do
    if snd x
        then (getAttackerWorth (fromList ys ! (fst x))) : getEarnedPoints xs ys
    else getEarnedPoints xs ys



























