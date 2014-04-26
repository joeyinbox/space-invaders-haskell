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
            let appData = (screen, (fontTitle, fontMenu, fontStatus), fontColor, resourceList)
            
            -- Declare and initialise all lists of variables needed within the same element to be able to pass it recursively
            let gameData = (hardResetGame, hardResetPlayer, (attackerIdList, resetAttackerTypeList, resetAttackerPositionList, resetAttackerAliveList, resetAttackerDirection), [], (bunkerIdList, resetBunkerTypeList, resetBunkerPositionList, resetBunkerStateList))
            
            -- Allow repeated keys
            enableKeyRepeat 10 10
    
            -- Call the loop function
            loop gameData appData





-- Handle all user events and key strokes
handleEvents gameData = do
    event <- pollEvent
    case event of
        -- If the user pressed a key
        (KeyDown (Keysym key _ _)) -> do
            case key of
                SDLK_ESCAPE -> return Dataset.Quit
                SDLK_RETURN -> do
                    if gameStateEq (getGameState gameData) MAIN || gameStateEq (getGameState gameData) GAMEOVER
                        then return Play
                        else return None
                SDLK_SPACE  -> do
                    if gameStateEq (getGameState gameData) INGAME
                        then return Shoot
                        else return None
                SDLK_LEFT   -> do
                    if gameStateEq (getGameState gameData) INGAME
                        then return MoveLeft
                        else return None
                SDLK_RIGHT  -> do
                    if gameStateEq (getGameState gameData) INGAME
                        then return MoveRight
                        else return None
                _           -> return None
        -- If the user decided to close the window or otherwise.. nothing
        Graphics.UI.SDL.Quit -> return Dataset.Quit
        NoEvent              -> return None
        _                    -> return None





-- The actual loop function
loop gameData appData = do
    -- Pause the app for 30ms (~33 fps)
    delay 30
    
    -- Display the right screen
    if gameStateEq (getGameState gameData) MAIN
        then displayMainScreen appData
    else if gameStateEq (getGameState gameData) GAMEOVER
        then displayGameOverScreen gameData appData
    else displayInGameScreen gameData appData
    
    
    -- Verify if the user triggered an event and therefore collect new data to apply
    result <- handleEvents gameData
    if eventResultEq result Dataset.Quit
        then putStr ""
    else do
        -- Update all game data!
        let newGameData = ((updateGameState result gameData,
                            updateGameActive result gameData,
                            updateLevel result gameData,
                            updateScore result gameData appData),
                           (updatePlayerLife result gameData appData,
                            updatePlayerPosition result gameData appData),
                           (getAttackerIdList gameData,                          -- TODO: useless to keep as there is already the global one?
                            getAttackerTypeList gameData,
                            updateAttackerPosition result gameData,
                            updateAttackerAliveList result gameData appData,
                            updateAttackerDirection result gameData),
                           updateBulletList result gameData appData,
                           (getBunkerIdList gameData,                            -- TODO: useless to keep as there is already the global one?
                            getBunkerTypeList gameData,
                            getBunkerPositionList gameData,
                            updateBunkerStateList result gameData))
        
        
        -- Call the loop
        loop newGameData appData




updateGameState result gameData = do
    if eventResultEq result Play 
        then INGAME 
    else if gameStateEq (getGameState gameData) INGAME && not (isGameActive gameData) 
        then GAMEOVER 
    else getGameState gameData


updateGameActive result gameData = do
    if eventResultEq result Play 
        then True
    else if gameStateEq (getGameState gameData) MAIN || gameStateEq (getGameState gameData) GAMEOVER
        then False
    else if (getPlayerLife gameData) <= 0
        then False
    else True


updateLevel result gameData = do
    if eventResultEq result Play 
        then 1
    else if length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        then (getLevel gameData)+1
    else getLevel gameData


updateScore result gameData appData = do
    if eventResultEq result Play 
        then 0
    else do
        -- Get the total of point remaining before and currently
        let former = sum (getEarnedPoints (getAttackerAliveList gameData) (getAttackerTypeList gameData))
        let current = sum (getEarnedPoints (keepUntouchedAttackerList (getAttackerAliveList gameData) (getBulletList gameData) gameData appData) (getAttackerTypeList gameData))
        
        -- Return the new score
        (getScore gameData)+(former-current)
    

updatePlayerLife result gameData appData = do
    if eventResultEq result Play 
        then 3
    else getPlayerLife gameData
    -- TODO: detect collision between bullet and player
    

updatePlayerPosition result gameData appData = do
    if eventResultEq result Play 
        then (190, 0)
    else if eventResultEq result MoveLeft && fst (getPlayerPosition gameData) >= 5 
        then ((fst (getPlayerPosition gameData)-5), snd (getPlayerPosition gameData))
    else if eventResultEq result MoveRight && fst (getPlayerPosition gameData) <= (1019-surfaceGetWidth (getPlayerImg appData))
        then ((fst (getPlayerPosition gameData)+5), snd (getPlayerPosition gameData))
    else getPlayerPosition gameData


updateAttackerPosition result gameData = do
    if eventResultEq result Play 
        then resetAttackerPositionList
    else moveAttackerDown                                       -- Apply an eventuel shift over attackers' Y position
           (moveAttackerSide                                       -- Apply an eventuel shift over attackers' X position
             (getAttackerPositionList gameData)                      -- The list on which the shift will be applied
             (getAttackerDirection gameData))                         -- Determine the value of the X shift (either -1 or 1) 
           (if detectTurn                                          -- Return True if the attackers reached the game limits
               (getAliveAttackerPositionList                            -- Return only the positions of the attackers which are still alive
                 (getAttackerAliveList gameData)                          -- The method getAliveAttackerPositionList needs the alive list
                 (getAttackerPositionList gameData))                      -- The method getAliveAttackerPositionList needs the position list
               (getAttackerDirection gameData)                            -- Determine the value of the X shift (either -1 or 1) 
               then 10 else 0)                                          -- Determine the value of the Y shift (either 0 or 10) according to the result of the previous method


updateAttackerAliveList result gameData appData = do
    if eventResultEq result Play 
        then resetAttackerAliveList
    else keepUntouchedAttackerList (getAttackerAliveList gameData) (getBulletList gameData) gameData appData


-- Change the direction of attackers if a turn is detected     
updateAttackerDirection result gameData = do
    if eventResultEq result Play 
        then resetAttackerDirection
    else if detectTurn
           (getAliveAttackerPositionList                        -- Return only the positions of the attackers which are still alive
               (getAttackerAliveList gameData)                    -- The method getAliveAttackerPositionList needs the alive list
               (getAttackerPositionList gameData))                -- The method getAliveAttackerPositionList needs the position list
           (getAttackerDirection gameData) 
         then (getAttackerDirection gameData)*(-1)            -- Invert it or..
    else (getAttackerDirection gameData)                      -- Keep the same


updateBulletList result gameData appData = do
    -- First, update all bullet positions
    let newBulletList = checkBulletPosition (updateBulletPosition (getBulletList gameData)) (surfaceGetHeight (getBulletImg appData))
    
    -- Then, check if the player wants to shoot
    if eventResultEq result Shoot 
        then addBullet newBulletList ((fst (getPlayerPosition gameData))+((surfaceGetWidth (getPlayerImg appData)) `quot` 2), (755-(surfaceGetHeight (getPlayerImg appData)))) (-1)
    else newBulletList


updateBunkerStateList result gameData = do
    if eventResultEq result Play 
        then resetBunkerStateList
    else getBunkerStateList gameData
    -- TODO: detect collision between bullet and bunker













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
	
	
	-- Display attackers
    displayAttacker (getAttackerTypeList gameData) appData (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData))
    
    -- TODO: Display an eventual spaceship
    
    -- Draw all bunkers
    displayBunker (getBunkerTypeList gameData) (getBunkerStateList gameData) appData (getBunkerPositionList gameData)
    
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


-- Display all bullets
displayBullet []         appData = putStr ""
displayBullet (x:xs) appData = do
    applySurface (fst (fst x)) (snd (fst x)) (getBulletImg appData) (getScreen appData)
    
    -- Loop
    displayBullet xs appData
    



















