module Bullet where

import Utils

-- Define Bullet list item (Position, Direction)
type Bullet = (Position, Int)

-- Add a bullet to the bullet list
addBullet :: [Bullet] -> Position -> Int -> [Bullet]
addBullet list position direction = (position, direction) : list

-- Remove a bullet from the bullet list via its position
removeBullet :: [Bullet] -> Position -> [Bullet]
removeBullet []     _ = []
removeBullet (x:xs) y = do
    if fst (fst x) == fst y && snd (fst x) == snd y 
        then removeBullet xs y
    else x : removeBullet xs y

-- (re-)Initialise the list of bullets
resetBulletList :: [a] -> [a]
resetBulletList _ = []