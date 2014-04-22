module Main where

import Control.Monad
import Graphics.UI.SDL
import Dataset
import Attacker
import Bullet
import Game
import Utils


-- Main function
main = withInit [InitEverything] $ do
	-- Initialise the window size with 32bits
    screen <- setVideoMode 1024 770 32 [SWSurface]
    
    -- Set the window title and the background
    setCaption "Space Intruders" []
    image <- loadImage "../res/img/background.png"
    
    -- Reflect these changes on the window and refresh it
    applySurface 0 0 image screen
    Graphics.UI.SDL.flip screen	
    
    -- Call the loop function which verify if the user wants to quit the app or continue to use it
    loop
	
  where
    -- Set some variables
    -- Declare and initialise all lists of variables needed within the same element to be able to pass it recursively
    gameData = (attackerIdList, resetAttackerTypeList [], resetAttackerPositionList [], resetAttackerAliveList [], resetAttackerDirection 0, [])
    
    -- Verify if the user triggers a quit event, otherwise continue to loop
    loop = do
        closeApplication <- handleEvents
        unless closeApplication loop
    
    -- Handle all user events and key strokes
    handleEvents = do
        event <- pollEvent
        case event of
            -- If the user pressed a key
            (KeyDown (Keysym key _ _)) -> do
                case key of
                    SDLK_ESCAPE -> return True
                    SDLK_RETURN -> return True
                    SDLK_SPACE  -> return True
                    SDLK_LEFT   -> return True
                    SDLK_RIGHT  -> return True
                    _           -> return False
            -- If the user decided to close the window or otherwise.. nothing
            Quit    -> return True
            NoEvent -> return False
            _       -> handleEvents