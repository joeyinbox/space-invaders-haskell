module Dataset where

import Graphics.UI.SDL
import Graphics.UI.SDL.TTF.Types
import Data.Map
import Attacker
import Bullet
import Bunker
import Utils


-- Need to gather all game informations within a single tuple to allow recursion calls
-- 1.  game active?
-- 2.  level
-- 3.  score
-- 4.  player life
-- 5.  player position
-- 6.  list of attacker identifiers
-- 7.  list of attacker types
-- 8.  list of attacker positions
-- 9.  list of attacker state (alive or not)
-- 10. direction of the attackers
-- 11. list of bullets
-- 12. list of bunker part identifiers
-- 13. list of bunker part types
-- 14. list of bunker part positions
-- 15. list of bunker part state
--type GameDataType = ([Int], [(Int, AttackerType)], [(Int, Position)], [(Int, Bool)], Int, [Bullet])
type GameDataType = ((Bool, Int, Int), (Int, Position), ([Int], [(Int, AttackerType)], [(Int, Position)], [(Int, Bool)], Int), [Bullet], ([Int], [(Int, BunkerType)], [(Int, Position)], [(Int, BunkerState)]))




-- Retrieve informations within GameDataType
-- Get the active state of the game
isGameActive :: GameDataType -> Bool
isGameActive ((a,_,_),_,_,_,_) = a

-- Get the level
getLevel :: GameDataType -> Int
getLevel ((_,a,_),_,_,_,_) = a

-- Get the score
getScore :: GameDataType -> Int
getScore ((_,_,a),_,_,_,_) = a

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






-- Return a list of tuples (Int, Position) of all alive attackers
getAliveAttackerPositionList :: [(Int, Bool)] -> [(Int, Position)] -> [(Int, Position)]
getAliveAttackerPositionList [] _ = []
getAliveAttackerPositionList (x:xs) yys = do
    if snd x
        then (fst x,(fromList yys ! fst x)) : getAliveAttackerPositionList xs yys
    else getAliveAttackerPositionList xs yys





-- Need to gather all game informations within a single tuple to allow recursion calls
type AppDataType = (AppState, IO Surface, FontList, Color, SurfaceList)
data AppState = MAIN | INGAME | GAMEOVER
type FontList = (Font, Font, Font)
type SurfaceList = (IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, (IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface, IO Surface), IO Surface)

-- Allow to retrieve informations within AppDataType
getAppState (a,_,_,_,_) = a
getScreen (_,a,_,_,_) = a
getFontTitle (_,_,(a,_,_),_,_) = a
getFontMenu (_,_,(_,a,_),_,_) = a
getFontStatus (_,_,(_,_,a),_,_) = a
getFontColor (_,_,_,a,_) = a
getBackgroundImg (_,_,_,_,(a,_,_,_,_,_,_,_,_)) = a
getCrabImg (_,_,_,_,(_,a,_,_,_,_,_,_,_)) = a
getOctopusImg (_,_,_,_,(_,_,a,_,_,_,_,_,_)) = a
getSquidImg (_,_,_,_,(_,_,_,a,_,_,_,_,_)) = a
getSpaceshipImg (_,_,_,_,(_,_,_,_,a,_,_,_,_)) = a
getPlayerImg (_,_,_,_,(_,_,_,_,_,a,_,_,_)) = a
getBulletImg (_,_,_,_,(_,_,_,_,_,_,a,_,_)) = a
getBunkerTopLeft0Img (_,_,_,_,(_,_,_,_,_,_,_,(a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopLeft1Img (_,_,_,_,(_,_,_,_,_,_,_,(_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopLeft2Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopLeft3Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopRight0Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopRight1Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopRight2Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerTopRight3Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterLeft0Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterLeft1Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterLeft2Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterLeft3Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterRight0Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_),_)) = a
getBunkerCenterRight1Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_),_)) = a
getBunkerCenterRight2Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_),_)) = a
getBunkerCenterRight3Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_),_)) = a
getBunkerPlain0Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_),_)) = a
getBunkerPlain1Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_),_)) = a
getBunkerPlain2Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_),_)) = a
getBunkerPlain3Img (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_),_)) = a
getBunkerDestroyedImg (_,_,_,_,(_,_,_,_,_,_,_,(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a),_)) = a
getBaselineImg (_,_,_,_,(_,_,_,_,_,_,_,_,a)) = a

-- Define App State checking
appStateEq :: AppState -> AppState -> Bool
appStateEq MAIN     MAIN     = True
appStateEq INGAME   INGAME   = True
appStateEq GAMEOVER GAMEOVER = True
appStateEq _         _       = False



-- Hard Reset Game informations
hardResetGame :: (Bool, Int, Int)
hardResetGame = (True, 1, 0)












