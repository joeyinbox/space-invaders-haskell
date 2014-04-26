module Utils where

import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Control.Monad

import Graphics.UI.SDL
import Graphics.UI.SDL.Image


-- Load a picture file thanks to SDL
loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

-- Render a picture loaded as a surface on a givven surface (generally the screen)
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


-- Generate a fake random number based on game data
generateRandomNumber :: [(Int, Position)] -> Position -> Int -> Int
generateRandomNumber xxs p r = go r xxs p
    where
        go s []     p = (s+1337)
        go s (x:xs) p = go (s+(fst p)+((fst x)+1)+((fst (snd x))+1)+((snd (snd x))+1)) xs p


-- Get a speed factor to increase the movements of the attackers
getSpeedFactor :: Int -> Int -> Int
getSpeedFactor level now = ((now*level) `quot` 50) `quot` 1000