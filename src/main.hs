module Main where

import Data.Map
import Control.Monad
import Graphics.UI.SDL
import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render
import Dataset
import Attacker
import Player
import Bullet
import Bunker
import Game
import Utils


-- Main function
main = withInit [InitEverything] $ do
    -- Attempt to initialise TTF
    ttfFail <- TTFG.init
    if not ttfFail
        then putStrLn "Unable to load TTF"
        else do
        	-- Initialise the window size with 32bits
            screen <- setVideoMode 1024 770 32 [SWSurface]
    
            -- Set the window title
            setCaption "Space Intruders" []
            
            -- Load all fonts and the default color
            fontTitle <- openFont "../res/font/ca.ttf" 65
            fontMenu <- openFont "../res/font/vcr-osd-mono.ttf" 30
            fontStatus <- openFont "../res/font/synchro.ttf" 30
            let fontColor = Color 255 255 255
            
            -- Load all graphical resources
            background          <- loadImage "../res/img/background.png"
            crab                <- loadImage "../res/img/crab.png"
            octopus             <- loadImage "../res/img/octopus.png"
            squid               <- loadImage "../res/img/squid.png"
            spaceship           <- loadImage "../res/img/spaceship.png"
            player              <- loadImage "../res/img/player.png"
            bullet              <- loadImage "../res/img/bullet.png"
            bunkerTopLeft0      <- loadImage "../res/img/bunker/top-left-0.png"
            bunkerTopLeft1      <- loadImage "../res/img/bunker/top-left-1.png"
            bunkerTopLeft2      <- loadImage "../res/img/bunker/top-left-2.png"
            bunkerTopLeft3      <- loadImage "../res/img/bunker/top-left-3.png"
            bunkerTopRight0     <- loadImage "../res/img/bunker/top-right-0.png"
            bunkerTopRight1     <- loadImage "../res/img/bunker/top-right-1.png"
            bunkerTopRight2     <- loadImage "../res/img/bunker/top-right-2.png"
            bunkerTopRight3     <- loadImage "../res/img/bunker/top-right-3.png"
            bunkerCenterLeft0   <- loadImage "../res/img/bunker/center-left-0.png"
            bunkerCenterLeft1   <- loadImage "../res/img/bunker/center-left-1.png"
            bunkerCenterLeft2   <- loadImage "../res/img/bunker/center-left-2.png"
            bunkerCenterLeft3   <- loadImage "../res/img/bunker/center-left-3.png"
            bunkerCenterRight0  <- loadImage "../res/img/bunker/center-right-0.png"
            bunkerCenterRight1  <- loadImage "../res/img/bunker/center-right-1.png"
            bunkerCenterRight2  <- loadImage "../res/img/bunker/center-right-2.png"
            bunkerCenterRight3  <- loadImage "../res/img/bunker/center-right-3.png"
            bunkerPlain0        <- loadImage "../res/img/bunker/plain-0.png"
            bunkerPlain1        <- loadImage "../res/img/bunker/plain-1.png"
            bunkerPlain2        <- loadImage "../res/img/bunker/plain-2.png"
            bunkerPlain3        <- loadImage "../res/img/bunker/plain-3.png"
            bunkerDestroyed     <- loadImage "../res/img/bunker/destroyed.png"
            baseline            <- loadImage "../res/img/baseline.png"
            
            
            -- Gather all elements together to pass them as parameters
            let resourceList = (background, crab, octopus, squid, spaceship, player, bullet, (bunkerTopLeft0, bunkerTopLeft1, bunkerTopLeft2, bunkerTopLeft3, bunkerTopRight0, bunkerTopRight1, bunkerTopRight2, bunkerTopRight3, bunkerCenterLeft0, bunkerCenterLeft1, bunkerCenterLeft2, bunkerCenterLeft3, bunkerCenterRight0, bunkerCenterRight1, bunkerCenterRight2, bunkerCenterRight3, bunkerPlain0, bunkerPlain1, bunkerPlain2, bunkerPlain3, bunkerDestroyed), baseline)
            let appData = (MAIN, screen, (fontTitle, fontMenu, fontStatus), fontColor, resourceList)
            
            -- Display the main screen
            displayMainScreen appData
            
    
            -- Call the loop function which verify if the user wants to quit the app or continue to use it
            loop gameData appData
    
    where
        -- Declare and initialise all lists of variables needed within the same element to be able to pass it recursively
        --gameData = (attackerIdList, resetAttackerTypeList [], resetAttackerPositionList [], resetAttackerAliveList [], resetAttackerDirection 0, [])
        gameData = (hardResetGame, hardResetPlayer, (attackerIdList, resetAttackerTypeList [], resetAttackerPositionList [], resetAttackerAliveList [], resetAttackerDirection 0), [], (bunkerIdList, resetBunkerTypeList [], resetBunkerPositionList [], resetBunkerStateList []))
        
        -- The actual loop function
        loop gameData appData = do
            -- Pause the app for 30ms (~33 fps)
            delay 30
            
            -- Update data here
            -- Display changes here
            
            -- Verify if the user triggers an event
            closeApplication <- handleEvents gameData appData
            unless closeApplication (loop gameData appData)
        
        -- Handle all user events and key strokes
        handleEvents gameData appData = do
            event <- pollEvent
            case event of
                -- If the user pressed a key
                (KeyDown (Keysym key _ _)) -> do
                    case key of
                        SDLK_ESCAPE -> return True
                        SDLK_RETURN -> do
                            if appStateEq (Dataset.getAppState appData) MAIN || appStateEq (Dataset.getAppState appData) GAMEOVER
                                then (displayInGameScreen gameData appData)
                                else putStr ""
                            return False
                        SDLK_SPACE  -> do
                            if appStateEq (Dataset.getAppState appData) INGAME
                                then putStr "" -- Verify if the player can shoot
                                else putStr ""
                            return False
                        SDLK_LEFT   -> do
                            if appStateEq (Dataset.getAppState appData) INGAME
                                then putStr "" -- Verify if the player can go left
                                else putStr ""
                            return False
                        SDLK_RIGHT  -> do
                            if appStateEq (Dataset.getAppState appData) INGAME
                                then putStr "" -- Verify if the player can go right
                                else putStr ""
                            return False
                        _           -> return False
                -- If the user decided to close the window or otherwise.. nothing
                Quit    -> return True
                NoEvent -> return False
                _       -> return False



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
    txt <- renderTextSolid (getFontStatus appData) "LEVEL: XX" (getFontColor appData)
    applySurface 20 20 txt (getScreen appData)
	
	-- Score
    txt <- renderTextSolid (getFontStatus appData) "SCORE: XX" (getFontColor appData)
    applySurface 450 20 txt (getScreen appData)
	
	-- Life
    txt <- renderTextSolid (getFontStatus appData) "LIFE: XX" (getFontColor appData)
    applySurface (1004-surfaceGetWidth txt) 20 txt (getScreen appData)
	
	
	-- Display attackers
    displayAttacker (getAttackerTypeList gameData) appData (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData))
    
    -- Display an eventual spaceship
    
    -- Draw all bunkers
    displayBunker (getBunkerTypeList gameData) (getBunkerStateList gameData) appData (getBunkerPositionList gameData)
    
    -- Draw the bullets
    
    -- Draw the player
    applySurface (fst (getPlayerPosition gameData)) (755-(surfaceGetHeight (getPlayerImg appData))) (getPlayerImg appData) (getScreen appData)
    
    -- Draw the baseline
    applySurface 0 765 (getBaselineImg appData) (getScreen appData)
	
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
    
    -- Loop
    displayAttacker attackerTypeList appData xs


-- Display all bunkers
displayBunker bunkerTypeList bunkerStateList appData []     = putStr ""
displayBunker bunkerTypeList bunkerStateList appData (x:xs) = do
    -- Display by type
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
    
    
    
    -- Loop
    displayBunker bunkerTypeList bunkerStateList appData xs














