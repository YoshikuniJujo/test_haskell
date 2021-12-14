{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Game where

import Prelude hiding (Either(..))

import Foreign.C.Types
import Control.Arrow (first)
import Control.Monad
import Control.Monad.ST
import Data.List
import System.Random

import Human

-- PARAMETERS

landY :: CInt
landY = yFromBottom $ fieldHeight - 2

pointArea :: Position
pointArea = Position (fieldWidth - 20) 3

gameOverMessage :: Message
gameOverMessage = Message (Position 20 10) "G A M E   O V E R"

-- GAME STATE

data GameState = GameState {
	gameStateHero :: Hero,
	gameStateEnemies :: [Enemy], gameStateEnemyEnergy :: Int,
	gameStateRandomGen :: StdGen,
	gameStatePoint :: Int, gameStateFailure :: Bool } deriving Show

initGameState :: GameState
initGameState = GameState {
	gameStateHero = Hero {
		heroX = 0, heroXMilli = 0,
		heroRun = Stand, heroJumping = NotJump },
	gameStateEnemies = [], gameStateEnemyEnergy = 0,
	gameStateRandomGen = mkStdGen 8,
	gameStatePoint = 0, gameStateFailure = False }

gameDraw :: Field RealWorld -> GameState -> IO ()
gameDraw f GameState {
	gameStateHero = h, gameStateEnemies = es,
	gameStatePoint = pnt, gameStateFailure = flr } = do
	fieldClearBackgroundRaw False f
	putEnemy f `mapM_` es
	fieldPutHero f h
	fieldPutMessageRaw f . Message pointArea $ show pnt
	when flr $ fieldPutMessageRaw f gameOverMessage
	fieldDraw f

-- HERO

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

-- ENEMY

data Enemy = Enemy { enemyX :: CInt, enemyXMilli :: CInt } deriving Show

enemyLeftOver :: Enemy -> Bool
enemyLeftOver (Enemy x _) = x < 0 || x > 75

enemyLeft :: Enemy -> CInt -> Enemy
enemyLeft Enemy { enemyX = x, enemyXMilli = xm } dxm = Enemy {
	enemyX = x + (xm - dxm) `div` 100, enemyXMilli = (xm - dxm) `mod` 100 }

putEnemy :: Field RealWorld -> Enemy -> IO ()
putEnemy f (Enemy x _) = fieldPutVariousHuman f enemyLooks x landY

enemyLooks :: Human
enemyLooks = Human {
	humanHeadSize = LargeHead, humanLeftArm = UpArm, humanRightArm = UpArm }

-- INPUT

data Input = Tick | Left | Stop | Right | Jump deriving Show

gameInput :: GameState -> Input -> GameState
gameInput g@GameState {
	gameStateHero = h, gameStateEnemies = es, gameStateEnemyEnergy = ee,
	gameStateRandomGen = rg, gameStatePoint = p } = \case
	Tick -> let
		h' = heroStep h
		(eng, rg') = randomR (0, calcEnemyEnergy) rg
		(me, ee') = enemyEnergyAdd ee eng
		(es', bs) = partition (not . checkBeat h') es
		es'' = maybe es' (: es') me
		(es''', g') = enemyMove rg' es'' in
		g {	gameStateHero = h', gameStateEnemies = filter (not . enemyLeftOver) es''',
			gameStateFailure = checkOverlap h' `any` es'', gameStateEnemyEnergy = ee',
			gameStateRandomGen = g',
			gameStatePoint = p + 10 * length bs + maybe 0 (const 1) me }
	Left -> g { gameStateHero = heroLeft h }
	Stop -> g { gameStateHero = h { heroRun = Stand } }
	Right -> g { gameStateHero = heroRight h }
	Jump -> g { gameStateHero = heroJump h }
	where
	enemyEnergyAdd eng deng
		| eng + deng > 1000 =
			(Just $ Enemy 70 0, (eng + deng) `mod` 1000)
		| otherwise = (Nothing, eng + deng)

	calcEnemyEnergy
		| p < 60 = 20 + p `div` 10
		| p < 120 = 23 + p `div` 20
		| otherwise = 26 + p `div` 40

	enemyMove g_ [] = ([], g_)
	enemyMove g_ (e : es_) = let
		(dex, g') = randomR (- 10, 65) g_ in first (enemyLeft e dex :) $ enemyMove g' es_

	checkBeat h_ e@(Enemy _ _) = checkOverlap h_ e && getHeroY h_ < landY

	checkOverlap h_ (Enemy ex _) = (el <= hr && hl <= er) && (et <= hb && ht <= eb)
		where
		hx = heroX h_
		hy = getHeroY h_
		[hl, hr, ht, hb] = map (\f -> f hx hy) [left, right, top, bottom]
		ey = landY
		[el, er, et, eb] = map (\f -> f ex ey) [left, right, top, bottom]
