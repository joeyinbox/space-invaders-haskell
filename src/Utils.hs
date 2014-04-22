module Utils where

import Control.Concurrent (threadDelay)
import Data.Time.Clock

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
repeatNTimes 0 _ = return ()
repeatNTimes n action = do
    action
    repeatNTimes (n-1) action

-- Sleep for 500ms (~2fps)
tick :: IO ()
tick = do t <- getCurrentTime
          let secs = round (realToFrac $ utctDayTime t) `rem` 100 -- 100
          threadDelay $ 5000 * (100 - secs) -- 1000 100