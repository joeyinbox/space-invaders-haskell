module Game where

import Data.Map
import Dataset
import Attacker
import Bullet
import Utils



--3
--take 3*11 list                              --> list des 33 premiers
--flip (take 3*11 list)                       --> list des 33 premiers inversés
--take 11 (flip (take 3*11 list))             --> list de 23 à 33 inversés
--flip (take 11 (flip (take 3*11 list)))
--
--take 11 list






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



-- Function called at each tick every 100ms (~10fps)
loop xxs@(x:xs) gameData = do
    -- Update informations
    let newGameData =  -- Ids
                       (getIdList gameData, 
                       -- Types
                       getAttackerTypeList gameData, 
                       -- Positions
                       moveAttackerDown                                       -- Apply an eventuel shift over attackers' Y position
                         (moveAttackerSide                                       -- Apply an eventuel shift over attackers' X position
                           (getAttackerPositionList gameData) 
                           (getAttackerDirection gameData)                    -- Determine the value of the X shift (either -1 or 1)
                         ) (if detectTurn                                    
                             (getAliveAttackerPositionList                     -- Return only the positions of the attackers which are still alive
                               (getAttackerAliveList gameData)                    -- The method getAliveAttackerPositionList needs the alive list
                               (getAttackerPositionList gameData)                 -- The method getAliveAttackerPositionList needs the position list
                             ) 
                             (getAttackerDirection gameData) then 1 else 0    -- Determine the value of the Y shift (either 0 or 1) according to the result of the previous method
                            ), 
                       -- Alives
                       getAttackerAliveList gameData, 
                       -- Direction
                       if detectTurn                                         -- Change the direction of attackers if a turn is detected                                  
                           (getAliveAttackerPositionList                       -- Return only the positions of the attackers which are still alive
                               (getAttackerAliveList gameData)                    -- The method getAliveAttackerPositionList needs the alive list
                               (getAttackerPositionList gameData)                 -- The method getAliveAttackerPositionList needs the position list
                            )  
                           (getAttackerDirection gameData) 
                         then (getAttackerDirection gameData)*(-1)            -- Invert it or..
                         else (getAttackerDirection gameData),                -- Keep the same
                       -- Bullets
                       getBulletList gameData)
    
    -- Clear the screen
    clearScreen
    
    -- Display informations
    display newGameData
    
    -- Sleep for 500ms (~2fps)
    tick
    
    -- Loop again if the game is not over
    if not (detectGameOver (getAttackerPositionList newGameData))
        then loop xs newGameData
    else putStrLn "Game Over"



-- Clear the screen
clearScreen = do
    repeatAction 100 (putStrLn "")


-- Display the whole game
display gameData = do
    -- Display the statistics
    putStrLn "------------------------------------------------------------------------------------------------------"
    putStrLn "| Level: X                                    Score: X                                      Lives: X |"
    -- Fill the gap between the top and the attackers
    repeatAction ((snd (fromList (getAttackerPositionList gameData) ! 1))-1) (putStrLn "|                                                                                                    |")
    
    -- Display all the attackers
    displayAllAttacker gameData ((concatMap (replicate 2) [1..5]) `zip` (cycle [1,2]))
    
    -- Fill the gap between the bottom and the attackers
    repeatAction (33-(snd (fromList (getAttackerPositionList gameData) ! 55))) (putStrLn "|                                                                                                    |")
    
    
    putStrLn "------------------------------------------------------------------------------------------------------"
    
    
    --
    --let id = 23
    --print (getLeftBoundary (getAttackerPositionList gameData))
    --print (getRightBoundary (getAttackerPositionList gameData))
    --print (getDownBoundary (getAttackerPositionList gameData))
    --print (getAttackerWorth (fromList (getAttackerTypeList gameData) ! id))
    --print (fst (fromList (getAttackerPositionList gameData) ! id))
    --print (snd (fromList (getAttackerPositionList gameData) ! id))
    --
    --if (fromList (getAttackerAliveList gameData) ! id) then
    --    putStrLn "Still alive"
    --else
    --    putStrLn "Destroyed"
    --
    --if attackerTypeEq Crab (fromList (getAttackerTypeList gameData) ! id) then
    --    putStrLn "It is a crab!"
    --else
    --    putStrLn "Not a crab.."
    


-- Display recursively a whole line of alive attackers
displayAttackerLine gameData [] _ = putStr ""
displayAttackerLine gameData (x:xs) line = do
    -- Display the attacker if it is still alive
    let id = (((fst line)-1)*11)+x
    
    if (fromList (getAttackerAliveList gameData) ! id) then
        if snd line == 1
            then putStr (fst (getAttackerGraphic (fromList (getAttackerTypeList gameData) ! id)))
        else if snd line == 2
            then putStr (snd (getAttackerGraphic (fromList (getAttackerTypeList gameData) ! id)))
        else putStr "    "
    else
        putStr "    "
    
    -- Display a margin if it is not the last attacker to process
    if length xs > 0 
        then putStr "   "
    else putStr ""
    
    -- Loop
    displayAttackerLine gameData xs line


-- Display all attackers, margins and the board borders
displayAllAttacker gameData []     = putStr ""
displayAllAttacker gameData (x:xs) = do
    -- Display the left border
    putStr "|"
    -- Display the space between the left side of a line and the first attacker alive
    repeatAction (getLeftBoundaryPerLine (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (fst x)) (putStr " ")
    -- Display attackers for this line
    displayAttackerLine gameData [1..11] x
    -- Display the space between the right side of a line and the last attacker alive
    repeatAction (96-(getRightBoundaryPerLine (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (fst x))) (putStr " ")
    -- Display the right border
    putStrLn "|"
    
    if snd x == 2
        then putStrLn "|                                                                                                    |"
    else putStr ""
    
    -- Loop
    displayAllAttacker gameData xs
    




    
    
    
    
    
    
    