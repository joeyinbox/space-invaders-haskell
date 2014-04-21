{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

-- The app will return void
main :: IO ()
main = do
    -- Initialise a new focus group to handle key events
    fg <- newFocusGroup
    
    -- Display a centered text
    b <- centered =<< (((((((((((((plainText " __   __        __   ___           ___  __        __   ___  __   __  ") 
                            <--> (plainText "/__` |__)  /\\  /  ` |__     | |\\ |  |  |__) |  | |  \\ |__  |__) /__` "))
                            <--> (plainText ".__/ |    /~~\\ \\__, |___    | | \\|  |  |  \\ \\__/ |__/ |___ |  \\ .__/ "))
                            <--> (plainText "                          /***\\ = ? Mystery") >>= withBoxSpacing 4)
                            <--> (plainText "                          ^‾^‾^"))
                            <--> (plainText "                           /ÒÓ\\ = 30 Points") >>= withBoxSpacing 1)
                            <--> (plainText "                            \\/"))
                            <--> (plainText "                           /MM\\ = 20 Points") >>= withBoxSpacing 1)
                            <--> (plainText "                           \\~~/"))
                            <--> (plainText "                            oÔo = 10 Points") >>= withBoxSpacing 1)
                            <--> (plainText "                            ^ ^"))
                            <--> (plainText "                         Press ENTER to start") >>= withBoxSpacing 4)
                            <--> (plainText "                    Press ESC to quit at any time") >>= withBoxSpacing 1
                        )

    coll <- newCollection
    _ <- addToCollection coll b fg

    -- handle key pressed!
    fg `onKeyPressed` \_ k _ ->
        case k of
            KEsc -> shutdownUi >> return True
            KEnter -> shutdownUi >> return True
            KASCII ' ' -> shutdownUi >> return True
            KLeft -> shutdownUi >> return True
            KRight -> shutdownUi >> return True
            _ -> return False

    runUi coll $ defaultContext { focusAttr = fgColor green }