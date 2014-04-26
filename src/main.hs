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
import Spaceship
import Game
import Utils
import Gui


-- Main function automatically executed when the app is launched
main = withInit [InitEverything] $ do
    -- Attempt to initialise TTF to display texts within the UI
    ttfFail <- TTFG.init
    if not ttfFail
        then putStrLn "Unable to load TTF"
    else do
    	-- Initialise the window size with 32bits
        screen <- setVideoMode 1024 770 32 [SWSurface]

        -- Set the window title
        setCaption "Space Intruders" []
        
        -- Load all fonts and the default color that will be used for texts
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
        let gameData = (hardResetGame, hardResetPlayer, (attackerIdList, resetAttackerTypeList, resetAttackerPositionList, resetAttackerAliveList, resetAttackerDirection), [], (bunkerIdList, resetBunkerTypeList, resetBunkerPositionList, resetBunkerStateList), resetSpaceship)
        
        -- Allow repeated keystrokes
        enableKeyRepeat 10 10

        -- Call the loop function
        loop gameData appData




-- Handle all user events and keystrokes
-- This will return a ResultEvent value based on the actions of the user
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




-- The actual loop function keeping the game running until the user decide to quit
loop gameData appData = do
    -- Pause the app for 30ms (~33 fps)
    delay 30
    
    -- Display the screen corresponding to the current state of the game
    if gameStateEq (getGameState gameData) MAIN
        then displayMainScreen appData
    else if gameStateEq (getGameState gameData) GAMEOVER
        then displayGameOverScreen gameData appData
    else displayInGameScreen gameData appData
    
    -- Get an eventual event triggered by the user
    result <- handleEvents gameData
    
    -- If the user decide to quit the game, let him do so
    if eventResultEq result Dataset.Quit
        then putStr ""
    else do
        -- Update all game data!
        let newGameData = ((updateGameState result gameData,
                            updateGameActive result gameData,
                            updateLevel result gameData,
                            updateScore result gameData appData,
                            updateTimestamp result gameData),
                           (updatePlayerLife result gameData appData,
                            updatePlayerPosition result gameData appData),
                           (getAttackerIdList gameData,
                            getAttackerTypeList gameData,
                            updateAttackerPosition result gameData,
                            updateAttackerAliveList result gameData appData,
                            updateAttackerDirection result gameData),
                           updateBulletList result gameData appData,
                           (getBunkerIdList gameData,
                            getBunkerTypeList gameData,
                            getBunkerPositionList gameData,
                            updateBunkerStateList result gameData),
                           (updateSpaceshipActive gameData appData,
                            updateSpaceshipPosition gameData appData,
                            updateSpaceshipDirection gameData,
                            updateSpaceshipWorth gameData))
        
        -- Call the loop again with the updated game data
        loop newGameData appData




-- The following functions are used to update pieces of the whole game data
-- Update the game state to display the correct screen
updateGameState result gameData = do
    if eventResultEq result Play 
        -- The user wants to start a new game
        then INGAME 
    else if gameStateEq (getGameState gameData) INGAME && not (isGameActive gameData) 
        -- The game is not active anymore which means that the game is over
        then GAMEOVER 
    else getGameState gameData


-- Update the fact that the game is active or not
updateGameActive result gameData = do
    if eventResultEq result Play 
        -- The user wants to start a new game
        then True
    else if gameStateEq (getGameState gameData) MAIN || gameStateEq (getGameState gameData) GAMEOVER
        -- The user is not currently playing
        then False
    else if (getPlayerLife gameData) <= 0 || detectGameOver (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData))
        -- The game is over because of a lack of remaining lives or because the attackers landed
        then False
    else True


-- Update the current level of the game
updateLevel result gameData = do
    if eventResultEq result Play 
        -- The user wants to start a new game
        then 1
    else if length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        -- The end of a wave is detected. Therefore, the level is increased
        then (getLevel gameData)+1
    else getLevel gameData


-- Update the score of the current game
updateScore result gameData appData = do
    if eventResultEq result Play 
        -- The user wants to start a new game
        then 0
    else do
        -- Get the total of point remaining before and currently
        let former = (sum (getEarnedPoints (getAttackerAliveList gameData) (getAttackerTypeList gameData)))
        let current = sum (getEarnedPoints (keepUntouchedAttackerList (getAttackerAliveList gameData) (getBulletList gameData) gameData appData) (getAttackerTypeList gameData))
        
        -- Detect if the spaceship has just been touched
        let space = if isSpaceshipActive gameData && detectTouchedSpaceship (getSpaceshipPosition gameData) (getBulletList gameData) (isSpaceshipActive gameData) (surfaceGetWidth (getSpaceshipImg appData), surfaceGetHeight (getSpaceshipImg appData))
                        then getSpaceshipWorth gameData 
                    else 0
        
        -- Return the new score
        (getScore gameData)+(former+space-current)


-- Update the current wave timestamp used to increase the speed factor
updateTimestamp result gameData = do
    if eventResultEq result Play || length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        -- The user wants to start a new game or has just finished a wave
        then 0
    else ((getTimestamp gameData)+30) -- The game is refresh every 30ms


-- Update the number of remaining lives of the player
updatePlayerLife result gameData appData = do
    if eventResultEq result Play 
        -- The user wants to start a new game
        then 3
    else if length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        -- The end of a wave is detected. Therefore, the player win an additional life
        then (getPlayerLife gameData)+1
    else if detectTouchedPlayer (getPlayerPosition gameData) (getBulletList gameData) (surfaceGetWidth (getPlayerImg appData), surfaceGetHeight (getPlayerImg appData))
        -- The player just got hurt by a bullet. He then lose a life
        then (getPlayerLife gameData)-1
    else getPlayerLife gameData


-- Update the position of the player
updatePlayerPosition result gameData appData = do
    if eventResultEq result Play 
        -- The user wants to start a new game
        then (190, 727)
    else if eventResultEq result MoveLeft && fst (getPlayerPosition gameData) >= 5 
        -- The player wants (and can) move to the left
        then ((fst (getPlayerPosition gameData)-5), snd (getPlayerPosition gameData))
    else if eventResultEq result MoveRight && fst (getPlayerPosition gameData) <= (1019-surfaceGetWidth (getPlayerImg appData))
        -- The player wants (and can) move to the right
        then ((fst (getPlayerPosition gameData)+5), snd (getPlayerPosition gameData))
    else getPlayerPosition gameData


-- Update the position of the attacker
updateAttackerPosition result gameData = do
    if eventResultEq result Play || length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        -- The user wants to start a new game or has just finished a wave
        then resetAttackerPositionList
    else moveAttackerDown                                       -- Apply an eventual shift over attackers' Y position
           (moveAttackerSide                                       -- Apply an eventual shift over attackers' X position
             (getAttackerPositionList gameData)                      -- The list on which the shift will be applied
             ((getAttackerDirection gameData)*(1+getSpeedFactor      -- Determine the value of the X shift (either -1 or 1 by default) 
               (getLevel gameData) (getTimestamp gameData))))          -- But the value is increased by the speed factor
           (if detectTurn                                          -- Return True if the attackers reached the game limits
               (getAliveAttackerPositionList                            -- Return only the positions of the attackers which are still alive
                 (getAttackerAliveList gameData)                          -- The list of all attackers alive
                 (getAttackerPositionList gameData))                      -- The list of all attacker positions
               ((getAttackerDirection gameData)*(1+getSpeedFactor      -- Determine the value of the X shift (either -1 or 1 by default) 
                 (getLevel gameData) (getTimestamp gameData)))          -- But the value is increased by the speed factor
               then 10 else 0)                                          -- Determine the value of the Y shift (either 0 or 10) according to the result of the previous method


-- Update the list of all remaining attackers alive
updateAttackerAliveList result gameData appData = do
    if eventResultEq result Play || length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        -- The user wants to start a new game or has just finished a wave
        then resetAttackerAliveList
    else keepUntouchedAttackerList (getAttackerAliveList gameData) (getBulletList gameData) gameData appData


-- Change the direction of attackers if a turn is detected     
updateAttackerDirection result gameData = do
    if eventResultEq result Play || length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        -- The user wants to start a new game or has just finished a wave
        then resetAttackerDirection
    else if detectTurn
           (getAliveAttackerPositionList                        -- Return only the positions of the attackers which are still alive
               (getAttackerAliveList gameData)                    -- The list of all attackers alive
               (getAttackerPositionList gameData))                -- The list of all attacker positions
           ((getAttackerDirection gameData)*(1+getSpeedFactor      -- Determine the value of the X shift (either -1 or 1 by default) 
             (getLevel gameData) (getTimestamp gameData)))          -- But the value is increased by the speed factor
         then (getAttackerDirection gameData)*(-1)            -- Invert it or..
    else (getAttackerDirection gameData)                      -- Keep the same


-- Update the list of all bullets
updateBulletList result gameData appData = do
    if length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        --  The player has just finished a wave
        then resetBulletList
    else do
        -- First, update all bullet positions on a restrained list of unexploded bullets
        let newBulletList = checkBulletPosition 
                              (updateBulletPosition 
                                (keepUnexplodedBulletListOnBunkerList 
                                  (keepUnexplodedBulletListOnAttackerList 
                                    (keepUnexplodedBulletListOnPlayer 
                                      (getPlayerPosition gameData) 
                                      (keepUnexplodedBulletListOnSpaceship 
                                        (getSpaceshipPosition gameData) 
                                        (getBulletList gameData) 
                                        (isSpaceshipActive gameData) 
                                        (surfaceGetWidth (getSpaceshipImg appData), surfaceGetHeight (getSpaceshipImg appData))) 
                                      (surfaceGetWidth (getPlayerImg appData), surfaceGetHeight (getPlayerImg appData))) 
                                    (getAliveAttackerPositionList 
                                      (getAttackerAliveList gameData) 
                                      (getAttackerPositionList gameData)) 
                                    gameData 
                                    appData) 
                                  (getUndestroyedBunkerPositionList 
                                    (getBunkerStateList gameData) 
                                    (getBunkerPositionList gameData)))) 
                                (surfaceGetHeight (getBulletImg appData))
    
        -- Then, check if the player wants to shoot and can do it
        if eventResultEq result Shoot && not (isPlayerBulletStillActive (getBulletList gameData))
            then addBullet newBulletList ((fst (getPlayerPosition gameData))+((surfaceGetWidth (getPlayerImg appData)) `quot` 2), (755-(surfaceGetHeight (getPlayerImg appData)))) (-1) True
        -- Or if the attackers will shoot randomly
        else if (rem (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) 100) >= 95
            then do
                makeAttackerShoot (getLowestAttackerPositionList (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData))) newBulletList (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) (getSpeedFactor (getLevel gameData) (getTimestamp gameData))
        else newBulletList


-- Update all bunker part states because of eventual impacts with bullets
updateBunkerStateList result gameData = do
    if eventResultEq result Play || length (getAliveAttackerList (getAttackerAliveList gameData)) == 0
        -- The user wants to start a new game or has just finished a wave
        then resetBunkerStateList
    else affectBunkerStateList (getBunkerStateList gameData) (getBunkerPositionList gameData) (getBulletList gameData)


-- Update the fact that the spaceship is active or not
updateSpaceshipActive gameData appData = do
    if isSpaceshipActive gameData && detectTouchedSpaceship (getSpaceshipPosition gameData) (getBulletList gameData) (isSpaceshipActive gameData) (surfaceGetWidth (getSpaceshipImg appData), surfaceGetHeight (getSpaceshipImg appData))
        -- The spaceship has just been touched by a bullet and must disappear
        then False
    else if isSpaceshipActive gameData && (fst (getSpaceshipPosition gameData) < -44 || fst (getSpaceshipPosition gameData) > 1068)
        -- The spaceship reached the other side of the window without beeing touched
        then False
    else if not (isSpaceshipActive gameData) && (getTimestamp gameData) > 0 && (rem (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) 1000) > 995
        -- Randomly, a spaceship can spawn if no other one is currently active during the game
        then True
    else isSpaceshipActive gameData


-- Update the position of the spaceship
updateSpaceshipPosition gameData appData = do
    if isSpaceshipActive gameData && ((getTimestamp gameData) == 0 || detectTouchedSpaceship (getSpaceshipPosition gameData) (getBulletList gameData) (isSpaceshipActive gameData) (surfaceGetWidth (getSpaceshipImg appData), surfaceGetHeight (getSpaceshipImg appData)))
        -- The spaceship has just been touched by a bullet or a new wave has just started. Therefore, it must be hidden
        then (-44, 100)
    else if isSpaceshipActive gameData
        -- Increase the position of the spaceship accordingly to the speed factor
        then ((fst (getSpaceshipPosition gameData))+((getSpaceshipDirection gameData)*((1+getSpeedFactor (getLevel gameData) (getTimestamp gameData))*2)), snd (getSpaceshipPosition gameData))
    else if not (isSpaceshipActive gameData) && (getTimestamp gameData) > 0 && (rem (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) 1000) > 995
        -- A new spaceship has just spawn. Therefore, it will randomly appear from the left or the right side of the screen
        then if (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) `quot` 2 == 0
            then (-44, 100)
        else (1068, 100)
    else getSpaceshipPosition gameData


-- Update the direction of the spaceship
updateSpaceshipDirection gameData = do
    if not (isSpaceshipActive gameData) && (getTimestamp gameData) > 0 && (rem (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) 1000) > 995
        -- A new spaceship has just spawn. Therefore, it will randomly move to the left or the right side of the screen
        -- As, the same timestamp is shared between all update functions, its direction will be in respect to its new position
        then if (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) `quot` 2 == 0
            then 1
        else -1
    else getSpaceshipDirection gameData


-- Update the value of the spaceship
updateSpaceshipWorth gameData = do
    if not (isSpaceshipActive gameData) && (getTimestamp gameData) > 0 && (rem (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) 1000) > 995
        -- A new spaceship has just spawn. Therefore, a new random value within a range is set
        then ((rem (generateRandomNumber (getAliveAttackerPositionList (getAttackerAliveList gameData) (getAttackerPositionList gameData)) (getPlayerPosition gameData) (getTimestamp gameData)) 5)+1)*50
    else getSpaceshipWorth gameData