{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Game where

import Foreign.C.Types
import Control.Monad.ST

import Human

data Event = Tick | Left | Stop | Right | Jump deriving Show

landY :: CInt
landY = c_hm_y_from_bottom $ fieldHeight - 2

data GameState = GameState {
	gameStateHero :: Hero,
	gameStateEnemies :: [Enemy] } deriving Show

initGameState :: GameState
initGameState = GameState {
	gameStateHero = Hero { heroX = 0, heroRun = Stand, heroJump = NotJump },
	gameStateEnemies = [] }

dummyGameState :: GameState
dummyGameState = GameState {
	gameStateHero = Hero { heroX = 0, heroRun = Forward, heroJump = NotJump },
	gameStateEnemies = [] }

gameDraw :: Field RealWorld -> GameState -> IO ()
gameDraw f GameState { gameStateHero = h, gameStateEnemies = _e } = do
	fieldClear f
	fieldPutHero f h
	fieldDraw f

gameEvent :: GameState -> Event -> GameState
gameEvent g@GameState { gameStateHero = h, gameStateEnemies = es } = \case
	Tick -> GameState { gameStateHero = heroStep h, gameStateEnemies = es }
	_ -> g

data Hero = Hero {
	heroX :: CInt,
	heroRun :: Run,
	heroJump :: Jump } deriving Show

data Run = BackDash | Backward | Stand | Forward | ForwardDash deriving Show

data Jump = NotJump | Jumping Int deriving Show

fieldPutHero :: Field RealWorld -> Hero -> IO ()
fieldPutHero f h@Hero { heroX = x } = fieldPutHuman f x $ getHeroY h

getHeroY :: Hero -> CInt
getHeroY Hero { heroJump = j } = case j of
	NotJump -> landY
	Jumping (fromIntegral @_ @Double . (`mod` 100) -> t) ->
		landY - round (t * (100 - t) / 400)

heroStep :: Hero -> Hero
heroStep h@Hero { heroX = x, heroRun = r, heroJump = _j } = let
	x' = case r of Forward -> x + 1; _ -> x
	in h { heroX = x' }

data Enemy = Enemy CInt deriving Show
