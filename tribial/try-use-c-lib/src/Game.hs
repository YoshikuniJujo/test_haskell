{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Game where

import Prelude hiding (Either(..))

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
	gameStateHero = Hero {
		heroX = 0, heroXMilli = 0,
		heroRun = Stand, heroJumping = NotJump },
	gameStateEnemies = [] }

dummyGameState :: GameState
dummyGameState = GameState {
	gameStateHero = Hero {
		heroX = 0, heroXMilli = 0,
		heroRun = Forward, heroJumping = NotJump },
	gameStateEnemies = [] }

gameDraw :: Field RealWorld -> GameState -> IO ()
gameDraw f GameState { gameStateHero = h, gameStateEnemies = _e } = do
	fieldClear f
	fieldPutHero f h
	fieldDraw f

gameEvent :: GameState -> Event -> GameState
gameEvent g@GameState { gameStateHero = h, gameStateEnemies = es } = \case
	Tick -> GameState { gameStateHero = heroStep h, gameStateEnemies = es }
	Left -> g { gameStateHero = heroLeft h }
	Jump -> g { gameStateHero = heroJump h }
	_ -> g

data Hero = Hero {
	heroX :: CInt, heroXMilli :: CInt,
	heroRun :: Run,
	heroJumping :: Jump } deriving Show

data Run = BackDash | Backward | Stand | Forward | ForwardDash deriving Show

data Jump = NotJump | Jumping Int deriving Show

fieldPutHero :: Field RealWorld -> Hero -> IO ()
fieldPutHero f h@Hero { heroX = x } = fieldPutHuman f x $ getHeroY h

getHeroY :: Hero -> CInt
getHeroY Hero { heroJumping = j } = case j of
	NotJump -> landY
	Jumping (fromIntegral @_ @Double . (`mod` 100) -> t) ->
		landY - round (t * (100 - t) / 400)

heroStep :: Hero -> Hero
heroStep h@Hero { heroRun = r, heroJumping = j } = let
	h' = case r of
		Backward -> heroBackward h 10
		Forward -> heroForward h 10
		_ -> h in
	h' { heroJumping = case j of
		NotJump -> NotJump
		Jumping t
			| t >= 99 -> NotJump
			| otherwise -> Jumping $ t + 1 }

heroForward :: Hero -> CInt -> Hero
heroForward h@Hero { heroX = x, heroXMilli = xm } dxm = h {
	heroX = x + (xm + dxm) `div` 100, heroXMilli = (xm + dxm) `mod` 100 }

heroBackward :: Hero -> CInt -> Hero
heroBackward h = heroForward h . negate

heroLeft :: Hero -> Hero
heroLeft h@Hero { heroRun = r } = case r of
	BackDash -> h { heroRun = BackDash }
	Backward -> h { heroRun = BackDash }
	_ -> h { heroRun = Backward }

heroJump :: Hero -> Hero
heroJump h@Hero { heroJumping = j } = h { heroJumping = case j of
	NotJump -> Jumping 0
	_ -> j }

data Enemy = Enemy CInt deriving Show
