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
	gameStateEnemies :: [Enemy],
	gameStateFailure :: Bool } deriving Show

initGameState :: GameState
initGameState = GameState {
	gameStateHero = Hero {
		heroX = 0, heroXMilli = 0,
		heroRun = Stand, heroJumping = NotJump },
	gameStateEnemies = [],
	gameStateFailure = False }

dummyGameState :: GameState
dummyGameState = GameState {
	gameStateHero = Hero {
		heroX = 0, heroXMilli = 0,
		heroRun = Stand, heroJumping = NotJump },
	gameStateEnemies = [Enemy 50 0, Enemy 30 0, Enemy 65 0],
	gameStateFailure = False }

doesGameFailure :: GameState -> Bool
doesGameFailure = gameStateFailure

gameDraw :: Field RealWorld -> GameState -> IO ()
gameDraw f GameState { gameStateHero = h, gameStateEnemies = es } = do
	fieldClear f
	putEnemy f `mapM_` es
	fieldPutHero f h
	fieldDraw f

putEnemy :: Field RealWorld -> Enemy -> IO ()
putEnemy f (Enemy x _) = fieldPutVariousHuman f enemy x landY

enemy :: Human
enemy = Human {
	humanHeadSize = LargeHead,
	humanLeftArm = UpArm,
	humanRightArm = UpArm }

gameEvent :: GameState -> Event -> GameState
gameEvent g@GameState { gameStateHero = h, gameStateEnemies = es } = \case
	Tick -> let h' = heroStep h; es' = filter (not . checkBeat h') es in
		g {	gameStateHero = h', gameStateEnemies = map (`enemyLeft` 10) es',
			gameStateFailure = checkOverlap h' `any` es' }
	Left -> g { gameStateHero = heroLeft h }
	Stop -> g { gameStateHero = h { heroRun = Stand } }
	Right -> g { gameStateHero = heroRight h }
	Jump -> g { gameStateHero = heroJump h }

checkBeat :: Hero -> Enemy -> Bool
checkBeat h e@(Enemy ex _) = checkOverlap h e && hb == et
	where
	hx = heroX h
	hy = getHeroY h
	hb = c_hm_bottom hx hy
	ey = landY
	et = c_hm_top ex ey

checkOverlap :: Hero -> Enemy -> Bool
checkOverlap h (Enemy ex _) = (el <= hr && hl <= er) && (et <= hb && ht <= eb)
	where
	hx = heroX h
	hy = getHeroY h
	[hl, hr, ht, hb] = map (\f -> f hx hy)
		[c_hm_left, c_hm_right, c_hm_top, c_hm_bottom]
	ey = landY
	[el, er, et, eb] = map (\f -> f ex ey)
		[c_hm_left, c_hm_right, c_hm_top, c_hm_bottom]

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
		BackDash -> heroBackward h 20
		Backward -> heroBackward h 10
		Stand -> h
		Forward -> heroForward h 10
		ForwardDash -> heroForward h 20 in
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

heroRight :: Hero -> Hero
heroRight h@Hero { heroRun = r } = case r of
	ForwardDash -> h { heroRun = ForwardDash }
	Forward -> h { heroRun = ForwardDash }
	_ -> h { heroRun = Forward }

heroJump :: Hero -> Hero
heroJump h@Hero { heroJumping = j } = h { heroJumping = case j of
	NotJump -> Jumping 0
	_ -> j }

data Enemy = Enemy { enemyX :: CInt, enemyXMilli :: CInt } deriving Show

enemyLeft :: Enemy -> CInt -> Enemy
enemyLeft Enemy { enemyX = x, enemyXMilli = xm } dxm = Enemy {
	enemyX = x + (xm - dxm) `div` 100, enemyXMilli = (xm - dxm) `mod` 100 }
