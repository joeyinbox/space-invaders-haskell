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


-- Check if a number is in a list
isInList :: Int -> [(Int, Position)] -> Bool
isInList _  []     = False
isInList id (x:xs) = do
    if id == fst x
        then True
    else isInList id xs
            
            
            
            
            
            
            
            
            