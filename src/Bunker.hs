module Bunker where

import Data.Map
import Utils

-- This list store the identifier of all bunker parts
bunkerIdList = [1..48]

-- Define Bunker types
data BunkerType = TopLeft | TopRight | CenterLeft | CenterRight | Plain

-- Define Bunker type checking
bunkerTypeEq :: BunkerType -> BunkerType -> Bool
bunkerTypeEq TopLeft     TopLeft     = True
bunkerTypeEq TopRight    TopRight    = True
bunkerTypeEq CenterLeft  CenterLeft  = True
bunkerTypeEq CenterRight CenterRight = True
bunkerTypeEq Plain       Plain       = True
bunkerTypeEq _           _           = False

-- Define Bunker state
data BunkerState = Initial | Minor | Partial | Major | Destroyed

-- Define Bunker state checking
bunkerStateEq :: BunkerState -> BunkerState -> Bool
bunkerStateEq Initial   Initial   = True
bunkerStateEq Minor     Minor     = True
bunkerStateEq Partial   Partial   = True
bunkerStateEq Major     Major     = True
bunkerStateEq Destroyed Destroyed = True
bunkerStateEq _         _         = False


-- (re-)Initialise the list of bunker types
resetBunkerTypeList :: [(Int, BunkerType)]
resetBunkerTypeList = bunkerIdList `zip` (take 48 (cycle ([TopLeft, Plain, Plain, TopRight, Plain, Plain, Plain, Plain, Plain, CenterLeft, CenterRight, Plain])))

-- (re-)Initialise the list of bunker positions
resetBunkerPositionList :: [(Int, Position)]
resetBunkerPositionList = do
    -- Horizontal positions
    let x =  (take 12 (cycle ([170, 194, 218, 242]))) ++ (take 12 (cycle ([366, 390, 414, 438]))) ++ (take 12 (cycle ([562, 586, 610, 634]))) ++ (take 12 (cycle ([758, 782, 806, 830])))

    -- Vertical positions
    let y = (take 48 (cycle ([600,600,600,600,624,624,624,624,648,648,648,648])))

    -- Combine both x and y then map it to all identifiers
    bunkerIdList `zip` (addPosition x y)

-- (re-)Initialise the list of bunker states
resetBunkerStateList :: [(Int, BunkerState)]
resetBunkerStateList = bunkerIdList `zip` (cycle [Initial])





-- Return a list of tuples (Int, Position) of all undestroyed bunker
getUndestroyedBunkerPositionList :: [(Int, BunkerState)] -> [(Int, Position)] -> [(Int, Position)]
getUndestroyedBunkerPositionList [] _ = []
getUndestroyedBunkerPositionList (x:xs) yys = do
    if bunkerStateEq (snd x) Destroyed
        then getUndestroyedBunkerPositionList xs yys
    else (fst x,(fromList yys ! fst x)) : getUndestroyedBunkerPositionList xs yys

























