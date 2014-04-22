module Utils where

import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Control.Monad
import Graphics.UI.SDL
import Graphics.UI.SDL.Image

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dest = blitSurface src Nothing dest offset
    where offset = Just Rect {rectX=x, rectY=y, rectW=0, rectH=0}



-- Define Position type (x, y)
type Position = (Int, Int)

-- Add new positions to an existing list
addPosition :: [Int] -> [Int] -> [Position]
addPosition []     []     = []
addPosition (x:xs) (y:ys) = (x, y) : addPosition xs ys


-- Merge 2 lists together
merge :: [a] -> [a] -> [a]
merge []     []     = []
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Repeat a function or an action n times
repeatAction 0 _ = return ()
repeatAction n action = do
    action
    repeatAction (n-1) action

-- Sleep for 500ms (~2fps)
tick :: IO ()
tick = do t <- getCurrentTime
          let secs = round (realToFrac $ utctDayTime t) `rem` 100 -- 100
          threadDelay $ 5000 * (100 - secs) -- 1000 100

-- Duplicate each element of a list
duplicate = (>>= replicate 2)