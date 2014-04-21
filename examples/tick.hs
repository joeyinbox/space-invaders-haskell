module Main where

import Control.Concurrent (threadDelay)
import Data.Time.Clock

score = "42"

-- Main function
main = do 
    --displayGame
    animate [1..]


-- Sleep for 100ms (~10fps)
tick :: IO ()
tick = do t <- getCurrentTime
          let secs = round (realToFrac $ utctDayTime t) `rem` 100
          threadDelay $ 1000 * (100 - secs)


-- Refresh the screen
animate xxs@(x:xs) = do
    clearScreen
    displayGame
    --repeatNTimes x (putStr " ")
    --putStrLn "####"
    tick
    animate xs


-- Clear the screen
clearScreen = do
    repeatNTimes 100 (putStrLn "")


-- Repeat a function or an action n times
repeatNTimes 0 _ = return ()
repeatNTimes n action = do
    action
    repeatNTimes (n-1) action



displayGame = do
    putStrLn "------------------------------------------------------------------------------------------------------"
    putStr "| Level: 1                                    Score: "
    putStr score
    putStrLn "                                     Lives: 3 |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                                                                                                    |"
    putStrLn "|  /***\\                                                                                             |"
    putStrLn "|  ^‾^‾^                                                                                             |"
    putStrLn "|                                                                                                    |"
    putStrLn "|              /ÒÓ\\   /ÒÓ\\   /ÒÓ\\   /ÒÓ\\   /ÒÓ\\   /ÒÓ\\   /ÒÓ\\   /ÒÓ\\   /ÒÓ\\   /ÒÓ\\   /ÒÓ\\            |"
    putStrLn "|               \\/     \\/     \\/     \\/     \\/     \\/     \\/     \\/     \\/     \\/     \\/             |"
    putStrLn "|                                                                                                    |"
    putStrLn "|              /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\            |"
    putStrLn "|              \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/            |"
    putStrLn "|                                                                                                    |"
    putStrLn "|              /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\   /MM\\            |"
    putStrLn "|              \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/   \\~~/            |"
    putStrLn "|                                                                                                    |"
    putStrLn "|              oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo             |"
    putStrLn "|              ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^             |"
    putStrLn "|                                                                                                    |"
    putStrLn "|              oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo    oÔo             |"
    putStrLn "|              ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^    ^ ^             |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                /MMM\\                /MMM\\                /MMM\\                /MMM\\                |"
    putStrLn "|                MMMMM                MMMMM                MMMMM                MMMMM                |"
    putStrLn "|                MM MM                MM MM                MM MM                MM MM                |"
    putStrLn "|                         |                                                                          |"
    putStrLn "|                                                                                                    |"
    putStrLn "|                       ==^==                                                                        |"
    putStrLn "------------------------------------------------------------------------------------------------------"
