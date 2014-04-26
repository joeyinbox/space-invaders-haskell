module Bullet where

import Utils

-- Define Bullet list item (Position, Direction)
type Bullet = (Position, (Int, Bool))

-- Add a bullet to the bullet list
addBullet :: [Bullet] -> Position -> Int -> Bool -> [Bullet]
addBullet list position direction fromPlayer = (position, (direction, fromPlayer)) : list

-- Remove a bullet from the bullet list via its position            TODO: to keep? used?
removeBullet :: [Bullet] -> Position -> [Bullet]
removeBullet []     _ = []
removeBullet (x:xs) y = do
    if fst (fst x) == fst y && snd (fst x) == snd y 
        then removeBullet xs y
    else x : removeBullet xs y

-- (re-)Initialise the list of bullets
resetBulletList :: [a] -> [a]
resetBulletList _ = []

-- Update the position of all bullets
updateBulletPosition :: [Bullet] -> [Bullet]
updateBulletPosition []     = []
updateBulletPosition (x:xs) = do
    (((fst (fst x)),(snd (fst x))+((fst (snd x))*15)), (snd x)) : updateBulletPosition xs

-- Check if the bullets got out of the screen limits and remove those which does
checkBulletPosition :: [Bullet] -> Int -> [Bullet]
checkBulletPosition []     _    = []
checkBulletPosition (x:xs) size = do
    if (snd (fst x)) < (0-size) || (snd (fst x)) > 1024
        then checkBulletPosition xs size
    else x : checkBulletPosition xs size