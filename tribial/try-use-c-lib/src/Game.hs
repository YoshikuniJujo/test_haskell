{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Game where

import Foreign.C.Types

data GameState = GameState {
	gameStateHero :: Hero,
	gameStateEnemies :: [Enemy] } deriving Show

data Hero = Hero {
	heroX :: CInt,
	heroRun :: Run,
	heroJump :: Jump } deriving Show

data Run = BackDash | Backward | Stop | Forward | ForwardDash deriving Show

data Jump = NotJump | Jumping Int deriving Show

getHeroY :: Hero -> CInt
getHeroY Hero { heroJump = j } = case j of
	NotJump -> 19
	Jumping (fromIntegral @_ @Double . (`mod` 100) -> t) ->
		19 - round (t * (100 - t) / 400)

data Enemy = Enemy CInt deriving Show
