module Main where

import Attacker
import Bullet
import Game
import Utils

-- Main function
main = do
    -- Declare and initialise all lists of variables needed within the same element to be able to pass it recursively
    let gameData = (attackerIdList, resetAttackerTypeList [], resetAttackerPositionList [], resetAttackerAliveList [], resetAttackerDirection 0, [])
    
    -- Call the game loop
    loop [1..] gameData