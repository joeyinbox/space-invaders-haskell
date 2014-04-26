module Gui where

import Data.Map

import Graphics.UI.SDL
import Graphics.UI.SDL.TTF.Render

import Attacker
import Bunker
import Dataset
import Utils


-- Display the main screen elements
displayMainScreen appData = do
    -- Display the background
    applySurface 0 0 (getBackgroundImg appData) (getScreen appData)
    
    -- Display the main title
    txt <- renderTextSolid (getFontTitle appData) "Space Intruders" (getFontColor appData)
    applySurface 150 100 txt (getScreen appData)
    
    
	-- Display points table caption
    txt <- renderTextSolid (getFontMenu appData) "*SCORE ADVANCE TABLE*" (getFontColor appData)
    applySurface 325 250 txt (getScreen appData)
	
	-- Display the spaceship points
    txt <- renderTextSolid (getFontMenu appData) "= ? MYSTERY" (getFontColor appData)
    applySurface 455 330 txt (getScreen appData)
	
	-- Display the octopus points
    txt <- renderTextSolid (getFontMenu appData) "= 30 POINTS" (getFontColor appData)
    applySurface 455 390 txt (getScreen appData)
	
	-- Display the crab points
    txt <- renderTextSolid (getFontMenu appData) "= 20 POINTS" (getFontColor appData)
    applySurface 455 450 txt (getScreen appData)
	
	-- Display the squid points
    txt <- renderTextSolid (getFontMenu appData) "= 10 POINTS" (getFontColor appData)
    applySurface 455 510 txt (getScreen appData)
	
	
	-- Display points table icons
    applySurface 390 330 (getSpaceshipImg appData) (getScreen appData)
    applySurface 390 390 (getOctopusImg appData) (getScreen appData)
    applySurface 390 450 (getCrabImg appData) (getScreen appData)
    applySurface 395 510 (getSquidImg appData) (getScreen appData)
	
	
	-- Display the instructions
    txt <- renderTextSolid (getFontMenu appData) "PRESS ENTER TO PLAY" (getFontColor appData)
    applySurface 345 700 txt (getScreen appData)
	
	-- Refresh the screen to show all elements
    Graphics.UI.SDL.flip (getScreen appData)




-- Display in game elements such as the statistics, attackers, the player or the baseline
displayInGameScreen gameData appData = do
    -- Display the background
    applySurface 0 0 (getBackgroundImg appData) (getScreen appData)
    
	-- Display the status
	-- Level
    txt <- renderTextSolid (getFontStatus appData) ("LEVEL: " ++ (show (getLevel gameData))) (getFontColor appData)
    applySurface 20 20 txt (getScreen appData)
	
	-- Score
    txt <- renderTextSolid (getFontStatus appData) ("SCORE: " ++ (show (getScore gameData))) (getFontColor appData)
    applySurface (512-((surfaceGetWidth txt) `quot` 2)) 20 txt (getScreen appData)
	
	-- Life
    txt <- renderTextSolid (getFontStatus appData) ("LIFE: " ++ (show (getPlayerLife gameData))) (getFontColor appData)
    applySurface (1004-surfaceGetWidth txt) 20 txt (getScreen appData)
	
    
    -- Draw all bunkers
    displayBunker (getBunkerTypeList gameData) (getBunkerStateList gameData) appData (getBunkerPositionList gameData)
	
	-- Display attackers
    displayAttacker (getAttackerTypeList gameData) appData (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData))
    
    -- Display an eventual spaceship
    applySurface (fst (getSpaceshipPosition gameData)) (snd (getSpaceshipPosition gameData)) (getSpaceshipImg appData) (getScreen appData)
    
    -- Draw the bullets
    displayBullet (getBulletList gameData) appData
    
    -- Draw the player
    applySurface (fst (getPlayerPosition gameData)) (755-(surfaceGetHeight (getPlayerImg appData))) (getPlayerImg appData) (getScreen appData)
    
    -- Draw the baseline
    applySurface 0 765 (getBaselineImg appData) (getScreen appData)
	
	-- Refresh the screen to show all elements
    Graphics.UI.SDL.flip (getScreen appData)




-- Display the game over screen elements
displayGameOverScreen gameData appData = do
    -- Display the background
    applySurface 0 0 (getBackgroundImg appData) (getScreen appData)
    
    -- Display the main title
    txt <- renderTextSolid (getFontTitle appData) "GAME OVER" (getFontColor appData)
    applySurface (512-((surfaceGetWidth txt) `quot` 2)) 100 txt (getScreen appData)
	
	
	-- Display the final score
    txt <- renderTextSolid (getFontMenu appData) "*FINAL SCORE*" (getFontColor appData)
    applySurface (512-((surfaceGetWidth txt) `quot` 2)) 250 txt (getScreen appData)
	
	-- Display the score
    txt <- renderTextSolid (getFontMenu appData) ((show (getScore gameData))++" POINTS") (getFontColor appData)
    applySurface (512-((surfaceGetWidth txt) `quot` 2)) 330 txt (getScreen appData)
	
	-- Display the level
    txt <- renderTextSolid (getFontMenu appData) ("LEVEL: "++(show (getLevel gameData))) (getFontColor appData)
    applySurface (512-((surfaceGetWidth txt) `quot` 2)) 390 txt (getScreen appData)
	
	
	-- Display the instructions
    txt <- renderTextSolid (getFontMenu appData) "PRESS ENTER TO PLAY AGAIN" (getFontColor appData)
    applySurface (512-((surfaceGetWidth txt) `quot` 2)) 700 txt (getScreen appData)
	
	-- Refresh the screen to show all elements
    Graphics.UI.SDL.flip (getScreen appData)





-- Display all attackers
displayAttacker attackerTypeList appData []     = putStr ""
displayAttacker attackerTypeList appData (x:xs) = do
    -- Display by type
    if attackerTypeEq (fromList attackerTypeList ! (fst x)) Crab
        then applySurface (fst (snd x)) (snd (snd x)) (getCrabImg appData) (getScreen appData)
    else if attackerTypeEq (fromList attackerTypeList ! (fst x)) Octopus
        then applySurface (fst (snd x)) (snd (snd x)) (getOctopusImg appData) (getScreen appData)
    else if attackerTypeEq (fromList attackerTypeList ! (fst x)) Squid
        then applySurface (fst (snd x)) (snd (snd x)) (getSquidImg appData) (getScreen appData)
    else applySurface (fst (snd x)) (snd (snd x)) (getSpaceshipImg appData) (getScreen appData)
    
    -- Loop through all remaining attackers to display
    displayAttacker attackerTypeList appData xs


-- Display all bunkers
displayBunker bunkerTypeList bunkerStateList appData []     = putStr ""
displayBunker bunkerTypeList bunkerStateList appData (x:xs) = do
    -- Display by type and by state
    -- A clever way would be better with pointers or an eval operator to reduce the amount of code
    if bunkerTypeEq (fromList bunkerTypeList ! (fst x)) TopLeft
        then if bunkerStateEq (fromList bunkerStateList ! (fst x)) Initial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerTopLeft0Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Minor
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerTopLeft1Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Partial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerTopLeft2Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Major
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerTopLeft3Img appData) (getScreen appData)
             else applySurface (fst (snd x)) (snd (snd x)) (getBunkerDestroyedImg appData) (getScreen appData)
             
    else if bunkerTypeEq (fromList bunkerTypeList ! (fst x)) TopRight
        then if bunkerStateEq (fromList bunkerStateList ! (fst x)) Initial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerTopRight0Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Minor
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerTopRight1Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Partial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerTopRight2Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Major
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerTopRight3Img appData) (getScreen appData)
             else applySurface (fst (snd x)) (snd (snd x)) (getBunkerDestroyedImg appData) (getScreen appData)
             
    else if bunkerTypeEq (fromList bunkerTypeList ! (fst x)) CenterLeft
        then if bunkerStateEq (fromList bunkerStateList ! (fst x)) Initial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerCenterLeft0Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Minor
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerCenterLeft1Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Partial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerCenterLeft2Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Major
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerCenterLeft3Img appData) (getScreen appData)
             else applySurface (fst (snd x)) (snd (snd x)) (getBunkerDestroyedImg appData) (getScreen appData)
             
    else if bunkerTypeEq (fromList bunkerTypeList ! (fst x)) CenterRight
        then if bunkerStateEq (fromList bunkerStateList ! (fst x)) Initial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerCenterRight0Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Minor
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerCenterRight1Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Partial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerCenterRight2Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Major
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerCenterRight3Img appData) (getScreen appData)
             else applySurface (fst (snd x)) (snd (snd x)) (getBunkerDestroyedImg appData) (getScreen appData)
             
    else if bunkerTypeEq (fromList bunkerTypeList ! (fst x)) Plain
        then if bunkerStateEq (fromList bunkerStateList ! (fst x)) Initial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerPlain0Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Minor
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerPlain1Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Partial
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerPlain2Img appData) (getScreen appData)
             else if bunkerStateEq (fromList bunkerStateList ! (fst x)) Major
                 then applySurface (fst (snd x)) (snd (snd x)) (getBunkerPlain3Img appData) (getScreen appData)
             else applySurface (fst (snd x)) (snd (snd x)) (getBunkerDestroyedImg appData) (getScreen appData)
    else applySurface (fst (snd x)) (snd (snd x)) (getBunkerDestroyedImg appData) (getScreen appData)
    
    -- Loop through all remaining bunker parts to display
    displayBunker bunkerTypeList bunkerStateList appData xs


-- Display all bullets
displayBullet []         appData = putStr ""
displayBullet (x:xs) appData = do
    applySurface (fst (fst x)) (snd (fst x)) (getBulletImg appData) (getScreen appData)
    
    -- Loop through all remaining bullets to display
    displayBullet xs appData