module Attacker where

import Utils


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

-- Define Graphic type (line1, line2)
type Graphic = ([Char], [Char])

-- Return the graphic lines of an Attacker type
getAttackerGraphic :: AttackerType -> Graphic
getAttackerGraphic t = do
    if attackerTypeEq Crab t 
        then ("/MM\\", "\\~~/")
    else if attackerTypeEq Octopus t 
        then ("/ÒÓ\\", " \\/ ")
    else if attackerTypeEq Squid t 
        then ("oÔo ", "^ ^ ")
    else if attackerTypeEq Spaceship t 
        then ("/***\\", "^‾^‾^")
    else ("", "")


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


-- (re-)Initialise the list of attacker types
resetAttackerTypeList :: [(Int, AttackerType)] -> [(Int, AttackerType)]
resetAttackerTypeList _ = merge ([1..11] `zip` (cycle [Squid])) (merge ([12..33] `zip` (cycle [Crab])) ([34..55] `zip` (cycle [Octopus])))

-- (re-)Initialise the list indicating if the attackers are still alive
resetAttackerAliveList :: [(Int, Bool)] -> [(Int, Bool)]
resetAttackerAliveList _ = attackerIdList `zip` (cycle [True])

-- (re-)Initialise the list of attackers position
resetAttackerPositionList :: [(Int, Position)] -> [(Int, Position)]
resetAttackerPositionList _ = do
    -- Attacker positions start at x:7 y:14
    -- There will be 7 characters between 2 columns (of attacker X positions) including their width of 4
    -- There will be 3 characters betwwen 2 rows (of attacker Y positions) including their height of 2
    -- Horizontal positions
    let x = take 55 (cycle (Prelude.map (*7) [1..11]))

    -- Vertical positions
    let y = [14,14,14,14,14,14,14,14,14,14,14,
             17,17,17,17,17,17,17,17,17,17,17,
             20,20,20,20,20,20,20,20,20,20,20,
             23,23,23,23,23,23,23,23,23,23,23,
             26,26,26,26,26,26,26,26,26,26,26]

    -- Combine both x and y then map it to all identifiers
    attackerIdList `zip` (addPosition x y)

-- (re-)Initialise attackers direction
resetAttackerDirection :: Int -> Int
resetAttackerDirection _ = 1

-- Shift all X positions by a given number
moveAttackerSide :: [(Int, Position)] -> Int -> [(Int, Position)]
moveAttackerSide xs z = Prelude.map (\(x,y) -> (x, (fst y+z, snd y))) xs

-- Shift all Y positions by a given number
moveAttackerDown :: [(Int, Position)] -> Int -> [(Int, Position)]
moveAttackerDown xs z = Prelude.map (\(x,y) -> (x, (fst y, snd y+z))) xs