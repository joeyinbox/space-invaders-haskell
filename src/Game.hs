module Game where

import Data.Map
import Attacker
import Bullet
import Utils
import Gui



-- Find the attackers' boundaries (the most on the left, the right and the bottom)
getLeftBoundary [] = error "The list is empty"
getLeftBoundary (x:xs) = go (fst (snd x)) xs
  where
    go m [] = m 
    go m (y:ys) = if m < (fst (snd y)) then go m ys else go (fst (snd y)) ys
    
getRightBoundary [] = error "The list is empty"
getRightBoundary (x:xs) = go (fst (snd x)) xs
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
    if direction == -1 && (getLeftBoundary positionList) == 2
        then True
    else if direction == 1 && (getRightBoundary positionList) == 95 -- 100 minus a width of 4 minus 1 because the call is made after moving
        then True
    else False

-- Detect if attackers have just landed
detectGameOver :: [(Int, Position)] -> Bool
detectGameOver positionList = do
    if getDownBoundary positionList == 32
        then True
    else False

-- Need to gather all informations within a single tuple to allow recursion calls
-- 1.  list of attacker identifiers
-- 2.  list of attacker types
-- 3.  list of attacker positions
-- 4.  list of attacker state (alive or not)
-- 5.  direction of the attackers
-- 6.  list of bullets
type GameDataType = ([Int], [(Int, AttackerType)], [(Int, Position)], [(Int, Bool)], Int, [Bullet])

-- Allow to retrieve informations within GameDataType
-- Get the list of id
getIdList :: GameDataType -> [Int]
getIdList (a,_,_,_,_,_) = a

-- Get the list of attacker types
getAttackerTypeList :: GameDataType -> [(Int, AttackerType)]
getAttackerTypeList (_,a,_,_,_,_) = a

-- Get the list of attacker position
getAttackerPositionList :: GameDataType -> [(Int, Position)]
getAttackerPositionList (_,_,a,_,_,_) = a

-- Get the list indicating if the attackers are still alive
getAttackerAliveList :: GameDataType -> [(Int, Bool)]
getAttackerAliveList (_,_,_,a,_,_) = a

-- Get the list indicating if the attackers are still alive
getAttackerDirection :: GameDataType -> Int
getAttackerDirection (_,_,_,_,a,_) = a

-- Get the list of bullets
getBulletList :: GameDataType -> [Bullet]
getBulletList (_,_,_,_,_,a) = a




-- Function called at each tick every 100ms (~10fps)
loop xxs@(x:xs) gameData = do
    -- Update informations
    let newGameData = (getIdList gameData, 
                       getAttackerTypeList gameData, 
                       moveDown (moveSide (getAttackerPositionList gameData) (getAttackerDirection gameData)) (if detectTurn (getAttackerPositionList gameData) (getAttackerDirection gameData) then 1 else 0), 
                       getAttackerAliveList gameData, 
                       if detectTurn (getAttackerPositionList gameData) (getAttackerDirection gameData) then (getAttackerDirection gameData)*(-1) else (getAttackerDirection gameData), 
                       getBulletList gameData)
    
    clearScreen
    
    -- Display informations
    
    --let id = (mod x 55)+1
    let id = 1
    print (getLeftBoundary (getAttackerPositionList newGameData))
    print (getRightBoundary (getAttackerPositionList newGameData))
    print (getDownBoundary (getAttackerPositionList newGameData))
    print (getAttackerWorth (fromList (getAttackerTypeList newGameData) ! id))
    print (fst (fromList (getAttackerPositionList newGameData) ! id))
    print (snd (fromList (getAttackerPositionList newGameData) ! id))
    
    putStrLn (fst (getAttackerGraphic (fromList (getAttackerTypeList newGameData) ! id)))
    putStrLn (snd (getAttackerGraphic (fromList (getAttackerTypeList newGameData) ! id)))
    
    if (fromList (getAttackerAliveList newGameData) ! id) then
        putStrLn "Still alive"
    else
        putStrLn "Destroyed"
    
    if attackerTypeEq Octopus (fromList (getAttackerTypeList newGameData) ! id) then
        putStrLn "It is an octopus!"
    else
        putStrLn "Not an octopus.."
    
    
    -- Sleep for 500ms (~2fps)
    tick
    
    -- Loop again if the game is not over
    if not (detectGameOver (getAttackerPositionList newGameData))
        then loop xs newGameData
    else putStrLn "Game Over"