module Dataset where

import Graphics.UI.SDL
import Graphics.UI.SDL.TTF.Types
import Data.Map
import Attacker
import Bullet
import Bunker
import Utils


data GameState = MAIN | INGAME | GAMEOVER

-- Define Game State checking
gameStateEq :: GameState -> GameState -> Bool
gameStateEq MAIN     MAIN     = True
gameStateEq INGAME   INGAME   = True
gameStateEq GAMEOVER GAMEOVER = True
gameStateEq _         _       = False


-- Need to gather all game informations within a single tuple to allow recursion calls
-- 1.  game state
-- 2.  game active?
-- 3.  level
-- 4.  score
-- 5.  player life
-- 6.  player position
-- 7.  list of attacker identifiers
-- 8.  list of attacker types
-- 9.  list of attacker positions
-- 10. list of attacker state (alive or not)
-- 11. direction of the attackers
-- 12. list of bullets
-- 13. list of bunker part identifiers
-- 14. list of bunker part types
-- 15. list of bunker part positions
-- 16. list of bunker part state
type GameDataType = ((GameState, Bool, Int, Int), (Int, Position), ([Int], [(Int, AttackerType)], [(Int, Position)], [(Int, Bool)], Int), [Bullet], ([Int], [(Int, BunkerType)], [(Int, Position)], [(Int, BunkerState)]))



-- Retrieve informations within GameDataType
-- Get the game state
getGameState :: GameDataType -> GameState
getGameState ((a,_,_,_),_,_,_,_) = a

-- Get the active state of the game
isGameActive :: GameDataType -> Bool
isGameActive ((_,a,_,_),_,_,_,_) = a

-- Get the level
getLevel :: GameDataType -> Int
getLevel ((_,_,a,_),_,_,_,_) = a

-- Get the score
getScore :: GameDataType -> Int
getScore ((_,_,_,a),_,_,_,_) = a

-- Get the player remaining lives count
getPlayerLife :: GameDataType -> Int
getPlayerLife (_,(a,_),_,_,_) = a

-- Get the player position
getPlayerPosition :: GameDataType -> Position
getPlayerPosition (_,(_,a),_,_,_) = a

-- Get the list of attacker identifiers
getAttackerIdList :: GameDataType -> [Int]
getAttackerIdList (_,_,(a,_,_,_,_),_,_) = a

-- Get the list of attacker types
getAttackerTypeList :: GameDataType -> [(Int, AttackerType)]
getAttackerTypeList (_,_,(_,a,_,_,_),_,_) = a

-- Get the list of attacker position
getAttackerPositionList :: GameDataType -> [(Int, Position)]
getAttackerPositionList (_,_,(_,_,a,_,_),_,_) = a

-- Get the list indicating if the attackers are still alive
getAttackerAliveList :: GameDataType -> [(Int, Bool)]
getAttackerAliveList (_,_,(_,_,_,a,_),_,_) = a

-- Get the attacker direction
getAttackerDirection :: GameDataType -> Int
getAttackerDirection (_,_,(_,_,_,_,a),_,_) = a

-- Get the list of bullets
getBulletList :: GameDataType -> [Bullet]
getBulletList (_,_,_,a,_) = a

-- Get the list of bunker part identifiers
getBunkerIdList :: GameDataType -> [Int]
getBunkerIdList (_,_,_,_,(a,_,_,_)) = a

-- Get the list of bunker part types
getBunkerTypeList :: GameDataType -> [(Int, BunkerType)]
getBunkerTypeList (_,_,_,_,(_,a,_,_)) = a

-- Get the list of bunker part position
getBunkerPositionList :: GameDataType -> [(Int, Position)]
getBunkerPositionList (_,_,_,_,(_,_,a,_)) = a

-- Get the list of bunker part state
getBunkerStateList :: GameDataType -> [(Int, BunkerState)]
getBunkerStateList (_,_,_,_,(_,_,_,a)) = a












-- Need to gather all game informations within a single tuple to allow recursion calls
type AppDataType = (IO Surface, FontList, Color, SurfaceList)
type FontList = (Font, Font, Font)
type SurfaceList = (IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, (IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface), IO Surface)

-- Allow to retrieve informations within AppDataType
getAppState (a,_,_,_,_) = a
getScreen (a,_,_,_) = a
getFontTitle (_,(a,_,_),_,_) = a
getFontMenu (_,(_,a,_),_,_) = a
getFontStatus (_,(_,_,a),_,_) = a
getFontColor (_,_,a,_) = a
getBackgroundImg (_,_,_,(a,_,_,_,_,_,_,_,_)) = a
getCrabImg (_,_,_,(_,a,_,_,_,_,_,_,_)) = a
getOctopusImg (_,_,_,(_,_,a,_,_,_,_,_,_)) = a
getSquidImg (_,_,_,(_,_,_,a,_,_,_,_,_)) = a
getSpaceshipImg (_,_,_,(_,_,_,_,a,_,_,_,_)) = a
getPlayerImg (_,_,_,(_,_,_,_,_,a,_,_,_)) = a
getBulletImg (_,_,_,(_,_,_,_,_,_,a,_,_)) = a
getBunkerTopLeft0Img (_,_,_,(_,_,_,_,_,_,_,(a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopLeft1Img (_,_,_,(_,_,_,_,_,_,_,(_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopLeft2Img (_,_,_,(_,_,_,_,_,_,_,(_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopLeft3Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopRight0Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopRight1Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopRight2Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopRight3Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterLeft0Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterLeft1Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterLeft2Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterLeft3Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterRight0Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterRight1Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_),_)) = a
getBunkerCenterRight2Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_),_)) = a
getBunkerCenterRight3Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_),_)) = a
getBunkerPlain0Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_),_)) = a
getBunkerPlain1Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_),_)) = a
getBunkerPlain2Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_),_)) = a
getBunkerPlain3Img (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_),_)) = a
getBunkerDestroyedImg (_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a),_)) = a
getBaselineImg (_,_,_,(_,_,_,_,_,_,_,_,a)) = a



-- Hard Reset Game informations
hardResetGame :: (GameState, Bool, Int, Int)
hardResetGame = (MAIN, False, 1, 0)


-- Define Event result type
data EventResult = MoveLeft | MoveRight | Shoot | Quit | Play | None

-- Define Event result checking
eventResultEq :: EventResult -> EventResult -> Bool
eventResultEq MoveLeft     MoveLeft     = True
eventResultEq MoveRight    MoveRight    = True
eventResultEq Shoot        Shoot        = True
eventResultEq Dataset.Quit Dataset.Quit = True
eventResultEq Play         Play         = True
eventResultEq None         None         = True
eventResultEq _            _            = False









