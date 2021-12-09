{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Game where

import Prelude hiding (Either(..))

import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.List
import System.Random

import Human

data Event = Tick | Left | Stop | Right | Jump deriving Show

landY :: CInt
landY = c_hm_y_from_bottom $ fieldHeight - 2

data GameState = GameState {
	gameStateHero :: Hero,
	gameStateEnemies :: [Enemy],
	gameStateEnemyEnergy :: Int,
	gameStateFailure :: Bool,
	gameStateRandomGen :: StdGen,
	gameStatePoint :: Int } deriving Show

initGameState :: GameState
initGameState = GameState {
	gameStateHero = Hero {
		heroX = 0, heroXMilli = 0,
		heroRun = Stand, heroJumping = NotJump },
	gameStateEnemies = [],
	gameStateEnemyEnergy = 0,
	gameStateFailure = False,
	gameStateRandomGen = mkStdGen 8,
	gameStatePoint = 0 }

dummyGameState :: GameState
dummyGameState = GameState {
	gameStateHero = Hero {
		heroX = 0, heroXMilli = 0,
		heroRun = Stand, heroJumping = NotJump },
	gameStateEnemies = [Enemy 50 0, Enemy 30 0, Enemy 65 0],
	gameStateEnemyEnergy = 0,
	gameStateFailure = False,
	gameStateRandomGen = mkStdGen 8,
	gameStatePoint = 0 }

doesGameFailure :: GameState -> Bool
doesGameFailure = gameStateFailure

gameDraw :: Field RealWorld -> GameState -> IO ()
gameDraw f GameState {
	gameStateHero = h, gameStateEnemies = es,
	gameStateFailure = flr, gameStatePoint = pnt } = do
	fieldClearBackgroundRaw False f
	putEnemy f `mapM_` es
	fieldPutHero f h
	fieldPutMessageRaw f . Message (Position 60 3) $ show pnt
	when flr . fieldPutMessageRaw f
		$ Message (Position 20 10) "G A M E   O V E R"
	fieldDraw f

putEnemy :: Field RealWorld -> Enemy -> IO ()
putEnemy f (Enemy x _) = fieldPutVariousHuman f enemy x landY

enemy :: Human
enemy = Human {
	humanHeadSize = LargeHead,
	humanLeftArm = UpArm,
	humanRightArm = UpArm }

enemyEnergyAdd :: Int -> Int -> (Maybe Enemy, Int)
enemyEnergyAdd eng deng
	| eng + deng > 1000 = (Just $ Enemy 70 0, (eng + deng) `mod` 1000)
	| otherwise = (Nothing, eng + deng)

gameEvent :: GameState -> Event -> GameState
gameEvent g@GameState {
	gameStateHero = h, gameStateEnemies = es, gameStateEnemyEnergy = ee,
	gameStateRandomGen = rg, gameStatePoint = p } = \case
	Tick -> let
		h' = heroStep h
--		(me, ee') = enemyEnergyAdd ee 11
		(eng, rg') = randomR (0, calcEnemyEnergy p) rg
		(me, ee') = enemyEnergyAdd ee eng
		(es', bs) = partition (not . checkBeat h') es
		es'' = maybe es' (: es') me
		(es''', g') = enemyMove rg' es'' in
--		(dex, g') = randomR (- 60, 100) rg in
		g {	gameStateHero = h', gameStateEnemies = filter (not . enemyLeftOver) es''', -- $ map (`enemyLeft` dex) es'',
			gameStateFailure = checkOverlap h' `any` es'', gameStateEnemyEnergy = ee',
			gameStateRandomGen = g',
			gameStatePoint = p + 10 * length bs + maybe 0 (const 1) me }
	Left -> g { gameStateHero = heroLeft h }
	Stop -> g { gameStateHero = h { heroRun = Stand } }
	Right -> g { gameStateHero = heroRight h }
	Jump -> g { gameStateHero = heroJump h }

calcEnemyEnergy :: Int -> Int
calcEnemyEnergy p
	| p < 200 = 20 + p `div` 20
	| otherwise = 25 + p `div` 40

enemyMove :: StdGen -> [Enemy] -> ([Enemy], StdGen)
enemyMove g [] = ([], g)
enemyMove g (e : es) = let
	(dex, g') = randomR (- 10, 65) g in first (enemyLeft e dex :) $ enemyMove g' es

checkBeat :: Hero -> Enemy -> Bool
checkBeat h e@(Enemy ex _) = checkOverlap h e && hy < ey -- && hb == et
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
	Jumping (fromIntegral @_ @Double . (`mod` 65) -> t) ->
		landY - round (t * (65 - t) / 200)

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
			| t >= 64 -> NotJump
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

enemyLeftOver :: Enemy -> Bool
enemyLeftOver (Enemy x _) =
	x < 0 || x > 75
