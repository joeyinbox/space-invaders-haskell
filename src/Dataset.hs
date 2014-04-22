module Dataset where

import Data.Map
import Attacker
import Bullet
import Utils

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


-- Return a list of tuples (Int, Position) of all alive attackers
getAliveAttackerPositionList :: [(Int, Bool)] -> [(Int, Position)] -> [(Int, Position)]
getAliveAttackerPositionList [] _ = []
getAliveAttackerPositionList (x:xs) yys = do
    if snd x
        then (fst x,(fromList yys ! fst x)) : getAliveAttackerPositionList xs yys
    else getAliveAttackerPositionList xs yys