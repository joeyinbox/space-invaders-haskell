module Game where

import Graphics.UI.SDL
import Data.Map
import Dataset
import Attacker
import Bullet
import Bunker
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
    if direction <= -1 && (getLeftBoundary positionList) <= 1
        then True
    else if direction >= 1 && (getRightBoundary positionList) >= 976
        then True
    else False

-- Detect if attackers have just landed
detectGameOver :: [(Int, Position)] -> Bool
detectGameOver positionList = do
    if getDownBoundary positionList >= 636
        then True
    else False




-- Mark all attackers which enter in collision with a bullet as not alive
--keepUntouchedAttackerList :: [(Int, Bool)] -> [(Position, (Int, Bool))] -> GameDataType -> AppDataType -> [(Int, Bool)]
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
detectTouchedAttacker :: (Int, Bool) -> [(Position, (Int, Bool))] -> (Int, Int) -> Position -> (Int, Bool)
detectTouchedAttacker x  []     _    _ = x
detectTouchedAttacker x  (y:ys) size pos = do
    -- Check if the current bullet position asserted corresponds to the position of the current attacker
    --    bullet X   >= attacker X&&    bullet X   <=   attacker X + width   &&    bullet Y   >= attacker Y&&    bullet Y   <=  attacker Y + height
    if fst (snd y) == -1 && (fst (fst y)) >= (fst pos) && (fst (fst y)) <= ((fst pos)+(fst size)) && (snd (fst y)) >= (snd pos) && (snd (fst y)) <= ((snd pos)+(snd size))
        then (fst x, False)
    else detectTouchedAttacker x ys size pos



-- Return the list of points available in a list of attackers
getEarnedPoints :: [(Int, Bool)] -> [(Int, AttackerType)] -> [Int]
getEarnedPoints []     _  = []
getEarnedPoints (x:xs) ys = do
    if snd x
        then (getAttackerWorth (fromList ys ! (fst x))) : getEarnedPoints xs ys
    else getEarnedPoints xs ys




-- Update the state of the parts of all bunkers if they touch a bullet
affectBunkerStateList :: [(Int, BunkerState)] -> [(Int, Position)] -> [(Position, (Int, Bool))] -> [(Int, BunkerState)]
affectBunkerStateList []     _  _  = []
affectBunkerStateList (x:xs) zs ys = affectBunkerState x ys (fromList zs ! (fst x)) : affectBunkerStateList xs zs ys


-- Update the state of one part of a bunker if it is touched by any of the bullet of the list
affectBunkerState :: (Int, BunkerState) -> [(Position, (Int, Bool))] -> Position -> (Int, BunkerState)
affectBunkerState x []     _   = x
affectBunkerState x (y:ys) pos = do
    -- Check if the current bullet position asserted corresponds to the position of the current bunker
    --   UP or DOWN         bullet X   >= bunker X  &&    bullet X   <=  bunker X + width &&  bullet Y  >= bunker Y  &&    bullet Y   <=  bunker Y + height
    if (fst (snd y) == (-1) && (fst (fst y)) >= (fst pos) && (fst (fst y)) <= ((fst pos)+24) && (snd (fst y)) >= (snd pos) && (snd (fst y)) <= ((snd pos)+24)) || 
       (fst (snd y) == (1) && (fst (fst y)) >= (fst pos) && (fst (fst y)) <= ((fst pos)+24) && ((snd (fst y))+18) >= (snd pos) && ((snd (fst y))+18) <= ((snd pos)+24))
        then do
            if bunkerStateEq (snd x) Initial
                then (fst x, Minor)
            else if bunkerStateEq (snd x) Minor
                then (fst x, Partial)
            else if bunkerStateEq (snd x) Partial
                then (fst x, Major)
            else if bunkerStateEq (snd x) Major
                then (fst x, Destroyed)
            else x
    else affectBunkerState x ys pos



-- Check if the player is touched by any of all the bullets
detectTouchedPlayer :: Position -> [(Position, (Int, Bool))] -> (Int, Int) -> Bool
detectTouchedPlayer x  []     _    = False
detectTouchedPlayer x  (y:ys) size = do
    -- Check if the current bullet position asserted corresponds to the position of the player
    --    DOWN                bullet X   >= player X  &&    bullet X   <=   player X + width     &&    bullet Y   >= player Y  &&    bullet Y   <=  player Y + height
    if fst (snd y) == 1 && (fst (fst y)) >= (fst x) && (fst (fst y)) <= ((fst x)+(fst size)) && (snd (fst y)) >= (snd x) && (snd (fst y)) <= ((snd x)+(snd size))
        then True
    else detectTouchedPlayer x ys size


-- Check if the spaceship is touched by any of all the bullets
detectTouchedSpaceship :: Position -> [(Position, (Int, Bool))] -> Bool -> (Int, Int) -> Bool
detectTouchedSpaceship x  []     _      _    = False
detectTouchedSpaceship x  (y:ys) active size = do
    if not active
        then False
    -- Check if the current bullet position asserted corresponds to the position of the player
    --             UP              bullet X   >= spaceship X  && bullet X  <=  spaceship X + width &&    bullet Y  >= spaceship Y &&  bullet Y <=  spaceship Y + height
    else if fst (snd y) == -1 && (fst (fst y)) >= (fst x) && (fst (fst y)) <= ((fst x)+(fst size)) && (snd (fst y)) >= (snd x) && (snd (fst y)) <= ((snd x)+(snd size))
        then True
    else detectTouchedSpaceship x ys active size











-- Keep only unexploded bullets facing attackers
keepUnexplodedBulletListOnAttackerList []     _   _ _ = []
keepUnexplodedBulletListOnAttackerList (x:xs) yys g a = (keepUnexplodedBulletListOnAttacker x yys g a) ++ (keepUnexplodedBulletListOnAttackerList xs yys g a)


-- Keep the bullet only if it hasn't exploded on any of the list of attackers
--keepUnexplodedBulletListOnAttacker :: (Position, (Int, Bool)) -> [(Int, Position)] -> GameDataType -> AppDataType -> [(Position, (Int, Bool))]
keepUnexplodedBulletListOnAttacker x  []     _ _ = [x]
keepUnexplodedBulletListOnAttacker x  (y:ys) g a = do
    -- Determine the attacker size
    let size = if attackerTypeEq (fromList (getAttackerTypeList g) ! (fst y)) Crab
                    then (surfaceGetWidth (getCrabImg a), surfaceGetHeight (getCrabImg a))
                else if attackerTypeEq (fromList (getAttackerTypeList g) ! (fst y)) Octopus
                    then (surfaceGetWidth (getOctopusImg a), surfaceGetHeight (getOctopusImg a))
                else if attackerTypeEq (fromList (getAttackerTypeList g) ! (fst y)) Squid
                    then (surfaceGetWidth (getSquidImg a), surfaceGetHeight (getSquidImg a))
                else (surfaceGetWidth (getSpaceshipImg a), surfaceGetHeight (getSpaceshipImg a))
    
    -- Check if the current bullet position asserted corresponds to the position of the current attacker
    --        UP               bullet X   >=  attacker X   &&    bullet X   <=    attacker X + width      &&    bullet Y   >=   attacker Y  &&    bullet Y   <=   attacker Y + height
    if fst (snd x) == -1 && (fst (fst x)) >= (fst (snd y)) && (fst (fst x)) <= ((fst (snd y))+(fst size)) && (snd (fst x)) >= (snd (snd y)) && (snd (fst x)) <= ((snd (snd y))+(snd size))
        then []
    else keepUnexplodedBulletListOnAttacker x ys g a





-- Keep only unexploded bullets facing bunkers
keepUnexplodedBulletListOnBunkerList []     _   = []
keepUnexplodedBulletListOnBunkerList (x:xs) yys = (keepUnexplodedBulletListOnBunker x yys) ++ (keepUnexplodedBulletListOnBunkerList xs yys)


-- Keep the bullet only if it hasn't exploded on any of the list of bunkers
--keepUnexplodedBulletListOnBunker :: (Position, (Int, Bool)) -> [(Int, Position)] -> [(Position, (Int, Bool))]
keepUnexplodedBulletListOnBunker x  []     = [x]
keepUnexplodedBulletListOnBunker x  (y:ys) = do
    -- Check if the current bullet position asserted corresponds to the position of the current bunker
    --   UP or DOWN         bullet X   >= bunker X  &&    bullet X   <=  bunker X + width &&  bullet Y  >= bunker Y  &&    bullet Y   <=  bunker Y + height
    if (fst (snd x) == (-1) && (fst (fst x)) >= (fst (snd y)) && (fst (fst x)) <= ((fst (snd y))+24) && (snd (fst x)) >= (snd (snd y)) && (snd (fst x)) <= ((snd (snd y))+24)) || 
       (fst (snd x) == (1) && (fst (fst x)) >= (fst (snd y)) && (fst (fst x)) <= ((fst (snd y))+24) && ((snd (fst x))+18) >= (snd (snd y)) && ((snd (fst x))+18) <= ((snd (snd y))+24))
        then []
    else keepUnexplodedBulletListOnBunker x ys



-- Keep only unexploded bullets facing the player
keepUnexplodedBulletListOnPlayer x []     _    = []
keepUnexplodedBulletListOnPlayer x (y:ys) size = do
    -- Check if the current bullet position asserted corresponds to the position of the player
    --      DOWN        &&   bullet X   >= player X  &&    bullet X   <=   player X + width     &&    bullet Y   >= player Y  &&    bullet Y   <=  player Y + height
    if fst (snd y) == 1 && (fst (fst y)) >= (fst x) && (fst (fst y)) <= ((fst x)+(fst size)) && (snd (fst y)) >= (snd x) && (snd (fst y)) <= ((snd x)+(snd size))
        then [] -- remove all bullets as the player has been touched
    else y : keepUnexplodedBulletListOnPlayer x ys size



-- Keep only unexploded bullets facing the spaceship
keepUnexplodedBulletListOnSpaceship x []     _      _    = []
keepUnexplodedBulletListOnSpaceship x yys@(y:ys) active size = do
    if not active
        then yys
    -- Check if the current bullet position asserted corresponds to the position of the spaceship
    --             UP              bullet X   >= spaceship X  && bullet X  <=  spaceship X + width &&    bullet Y  >= spaceship Y &&  bullet Y <=  spaceship Y + height
    else if fst (snd y) == -1 && (fst (fst y)) >= (fst x) && (fst (fst y)) <= ((fst x)+(fst size)) && (snd (fst y)) >= (snd x) && (snd (fst y)) <= ((snd x)+(snd size))
        then keepUnexplodedBulletListOnSpaceship x ys active size -- Remove this bullet
    else y : keepUnexplodedBulletListOnSpaceship x ys active size





-- Verify if the player bullet is still active to allow him to shoot again
isPlayerBulletStillActive :: [(Position, (Int, Bool))] -> Bool
isPlayerBulletStillActive [] = False
isPlayerBulletStillActive (y:ys) = do
    if snd (snd y)
        then True
    else isPlayerBulletStillActive ys


-- Generate a fake random number based on game data
generateRandomNumber :: [(Int, Position)] -> Position -> Int -> Int
generateRandomNumber xxs p r = go r xxs p
    where
        go s []     p = (s+1337)
        go s (x:xs) p = go (s+(fst p)+((fst x)+1)+((fst (snd x))+1)+((snd (snd x))+1)) xs p


-- Get a speed factor to increase the movements of the attackers
getSpeedFactor :: Int -> Int -> Int
getSpeedFactor level now = ((now*level) `quot` 50) `quot` 1000













