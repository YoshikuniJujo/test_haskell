{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Game where

import Prelude hiding (Either(..))

import Foreign.C.Types
import Control.Arrow (first, (&&&))
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

jumpDuration, jumpHeight :: Double
jumpDuration = 65; jumpHeight = 5

-- GAME STATE

data GameState = GameState {
	gameStateHero :: Hero,
	gameStateEnemies :: [Enemy], gameStateEnemyEnergy :: Int,
	gameStateRandomGen :: StdGen,
	gameStatePoint :: Int, gameStateFailure :: Bool } deriving Show

initGameState :: GameState
initGameState = GameState {
	gameStateHero = Hero {
		heroX = 0, heroXCenti = 0,
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
	putHero f h
	fieldPutMessageRaw f . Message pointArea $ show pnt
	when flr $ fieldPutMessageRaw f gameOverMessage
	fieldDraw f

-- HERO

data Hero = Hero {
	heroX :: CInt, heroXCenti :: CInt,
	heroRun :: Run, heroJumping :: Jump } deriving Show

data Run = BackDash | Backward | Stand | Forward | ForwardDash deriving Show
data Jump = NotJump | Jumping Double deriving Show

heroY :: Hero -> CInt
heroY (heroJumping -> j)= landY - round case j of
	NotJump -> 0
	Jumping t -> jumpHeight *
		t * (jumpDuration - t) / (jumpDuration / 2) ^ (2 :: Int)

heroForward, heroBackward :: Hero -> CInt -> Hero
heroForward hr@Hero { heroX = x, heroXCenti = xc } dxc =
	hr { heroX = x + d, heroXCenti = m }
	where (d, m) = (xc + dxc) `divMod` 100

heroBackward hr = heroForward hr . negate

putHero :: Field RealWorld -> Hero -> IO ()
putHero f = uncurry (fieldPutHuman f) . (heroX &&& heroY)

heroLeft, heroRight, heroJump :: Hero -> Hero
heroLeft hr@Hero { heroRun = r } = case r of
	BackDash -> hr { heroRun = BackDash }
	Backward -> hr { heroRun = BackDash }
	_ -> hr { heroRun = Backward }

heroRight hr@Hero { heroRun = r } = case r of
	ForwardDash -> hr { heroRun = ForwardDash }
	Forward -> hr { heroRun = ForwardDash }
	_ -> hr { heroRun = Forward }

heroJump hr@Hero { heroJumping = j } =
	hr { heroJumping = case j of NotJump -> Jumping 0; _ -> j }

heroStep :: Hero -> Hero
heroStep hr@Hero { heroRun = r, heroJumping = j } = let
	hr' = case r of
		BackDash -> heroBackward hr 20
		Backward -> heroBackward hr 10
		Stand -> hr
		Forward -> heroForward hr 10
		ForwardDash -> heroForward hr 20 in
	hr' { heroJumping = case j of
		NotJump -> NotJump
		Jumping t
			| t >= jumpDuration - 1 -> NotJump
			| otherwise -> Jumping $ t + 1 }

-- ENEMY

data Enemy = Enemy { enemyX :: CInt, enemyXCenti :: CInt } deriving Show

enemyOut :: Enemy -> Bool
enemyOut (Enemy x _) = left x landY < 0 || right x landY >= fieldWidth

putEnemy :: Field RealWorld -> Enemy -> IO ()
putEnemy f (enemyX -> x) = fieldPutVariousHuman f enemyLooks x landY
	where enemyLooks = Human {
		humanHeadSize = LargeHead,
		humanLeftArm = UpArm, humanRightArm = UpArm }

enemyForward :: Enemy -> CInt -> Enemy
enemyForward (enemyX &&& enemyXCenti -> (x, xc)) dxc =
	Enemy { enemyX = x + d, enemyXCenti = m }
	where (d, m) = (xc - dxc) `divMod` 100

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
		g {	gameStateHero = h', gameStateEnemies = filter (not . enemyOut) es''',
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
		(dex, g') = randomR (- 10, 65) g_ in first (enemyForward e dex :) $ enemyMove g' es_

	checkBeat h_ e@(Enemy _ _) = checkOverlap h_ e && heroY h_ < landY

	checkOverlap h_ (Enemy ex _) = (el <= hr && hl <= er) && (et <= hb && ht <= eb)
		where
		hx = heroX h_
		hy = heroY h_
		[hl, hr, ht, hb] = map (\f -> f hx hy) [left, right, top, bottom]
		ey = landY
		[el, er, et, eb] = map (\f -> f ex ey) [left, right, top, bottom]
