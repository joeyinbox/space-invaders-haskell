module Gui where

import Utils

-- Clear the screen
clearScreen = do
    repeatNTimes 100 (putStrLn "")