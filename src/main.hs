module Main where

import Data.Map
import System.Random


-- Define Attacker types
data AttackerType = Crab | Octopus | Squid | Spaceship

-- Define Attacker type checking
attackerTypeEq :: AttackerType -> AttackerType -> Bool
attackerTypeEq Crab      Crab      = True
attackerTypeEq Octopus   Octopus   = True
attackerTypeEq Squid     Squid     = True
attackerTypeEq Spaceship Spaceship = True
attackerTypeEq _         _         = False


-- Define Position type (x, y)
type Position = (Int, Int)

-- Add a new position to an existing list
addPosition :: [Int] -> [Int] -> [Position]
addPosition []     []     = []
addPosition (x:xs) (y:ys) = (x, y) : addPosition xs ys


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
-- For now, the value of the spaceship is fixed
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
            --n <- rand 1 6
            --return $ n*50
            --((randomRIO (1, 6))*50)  >>= (\x -> return x)
            --g <- newStdGen          -- intialise a random generator
            --let a = randoms g      -- one infinite list of randoms
            --return ((rem (head a) 6)+1)*50
    else 0



-- Merge 2 lists together
merge :: [a] -> [a] -> [a]
merge []     []     = []
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys




-- We now need the following variables:
-- Attackers id list
-- Tuple id, type (AttackerType)
-- Tuple id, position (Position)
-- Tuple id, alive (bool)


-- This list store the identifier of all attackers
attackerIdList = [1..55]


-- The complete list of attacker types
attackerTypeList = merge ([1..11] `zip` (cycle [Squid])) (merge ([12..33] `zip` (cycle [Crab])) ([34..55] `zip` (cycle [Octopus])))

-- Attacker positions start at x:7 y:14
-- There will be 7 characters between 2 columns (of attacker X positions) including their width of 4
-- There will be 3 characters betwwen 2 rows (of attacker Y positions) including their height of 2

-- 7  14  21  ... 77 for each row (5 times)
x = take 55 (cycle (Prelude.map (*7) [1..11]))

-- First 14     11 times
-- Then 17      11 times
-- etc.. until 26
y = [14,14,14,14,14,14,14,14,14,14,14,
     17,17,17,17,17,17,17,17,17,17,17,
     20,20,20,20,20,20,20,20,20,20,20,
     23,23,23,23,23,23,23,23,23,23,23,
     26,26,26,26,26,26,26,26,26,26,26]

-- The actual list of all attackers position
attackerPositionList = attackerIdList `zip` (addPosition x y)

-- Define the list indicating if the attackers are still alive
attackerAliveList = attackerIdList `zip` (cycle [True])








--move func list = do
    -- shift all x positions by func (either -1 or 1)
    -- verify if the biggest one (or lowest) touch the limit of the screen
        -- invert the direction
        
        
        -- get a PositionData aggregate to gather all positions and the direction
            -- so that one function (ex: move) can edit both at the same time and return it so that others can see the changes (ex: display)
        
        -- display function would then need that aggregate as a parameter in addition to other lists or eventual aggregates if needed for other functions
        
        
        -- Need a bullet list
            -- Tuple of Position (x,y) and Direction = UP | DOWN (with the Eq checker same as the AttackerType)











-- Main function
main = do 
    let id = 1
    print (getAttackerWorth (fromList attackerTypeList ! id))
    print (fst (fromList attackerPositionList ! id))
    
    putStrLn (fst (getAttackerGraphic (fromList attackerTypeList ! id)))
    putStrLn (snd (getAttackerGraphic (fromList attackerTypeList ! id)))
    
    if (fromList attackerAliveList ! id) then
        putStrLn "Still alive"
    else
        putStrLn "Destroyed"
    
    if attackerTypeEq Octopus (fromList attackerTypeList ! id) then
        putStrLn "It is an octopus!"
    else
        putStrLn "Not an octopus.."
    