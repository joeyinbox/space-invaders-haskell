module Main where

import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Data.Map


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

-- Add new positions to an existing list
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


-- Merge 2 lists together
merge :: [a] -> [a] -> [a]
merge []     []     = []
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Define Bullet list item (Position, Direction)
type Bullet = (Position, Int)

-- Add a bullet to the bullet list
addBullet :: [Bullet] -> Position -> Int -> [Bullet]
addBullet list position direction = (position, direction) : list

-- Retrieve a bullet from the bullet list via its position
--findBullet :: 

-- Remove a bullet from the bullet list via its position
removeBullet :: [Bullet] -> Position -> [Bullet]
removeBullet []     _ = []
removeBullet (x:xs) y = do
    if fst (fst x) == fst y && snd (fst x) == snd y 
        then removeBullet xs y
    else x : removeBullet xs y



-- We now need the following variables:
-- Attackers id list
-- Tuple id, type (AttackerType)
-- Tuple id, position (Position)
-- Tuple id, alive (bool)


-- This list store the identifier of all attackers
attackerIdList = [1..55]

-- (re-)Initialise the list of bullets
resetBulletList :: [a] -> [a]
resetBulletList _ = []

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
moveSide :: [(Int, Position)] -> Int -> [(Int, Position)]
moveSide xs z = Prelude.map (\(x,y) -> (x, (fst y+z, snd y))) xs

-- Shift all Y positions by a given number
moveDown :: [(Int, Position)] -> Int -> [(Int, Position)]
moveDown xs z = Prelude.map (\(x,y) -> (x, (fst y, snd y+z))) xs


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
    --loop xs newGameData




-- Main function
main = do
    -- Declare and initialise all lists of variables needed within the same element to be able to pass it recursively
    let gameData = (attackerIdList, resetAttackerTypeList [], resetAttackerPositionList [], resetAttackerAliveList [], resetAttackerDirection 0, [])
    
    -- Call the game loop
    loop [1..] gameData









-- Sleep for 500ms (~2fps)
tick :: IO ()
tick = do t <- getCurrentTime
          let secs = round (realToFrac $ utctDayTime t) `rem` 100 -- 100
          threadDelay $ 5000 * (100 - secs) -- 1000 100


-- Clear the screen
clearScreen = do
    repeatNTimes 100 (putStrLn "")


-- Repeat a function or an action n times
repeatNTimes 0 _ = return ()
repeatNTimes n action = do
    action
    repeatNTimes (n-1) action