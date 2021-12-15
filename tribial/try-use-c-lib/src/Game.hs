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
import Data.Bool
import Data.Maybe
import Data.List
import System.Random

import Human

-- PARAMETERS

pointArea :: Position
pointArea = Position (fieldWidth - 20) 3

gameOverMessage :: Message
gameOverMessage = Message (Position 20 10) "G A M E   O V E R"

landY :: CInt
landY = yFromBottom $ fieldHeight - 2

jumpDuration, jumpHeight :: Double
jumpDuration = 65; jumpHeight = 5

enemyInitX :: CInt
enemyInitX = xFromRight $ fieldWidth - 1

-- GAME STATE

data GameState = GameState {
	gameStateHero :: Hero,
	gameStateEnemies :: Enemies, gameStateEnemyEnergy :: EnemyEnergy,
	gameStateRandomGen :: StdGen,
	gameStatePoint :: Point, gameStateFailure :: Bool } deriving Show

type Point = Int

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
type EnemyEnergy = Int

enemyOut :: Enemy -> Bool
enemyOut (Enemy x _) = left x landY < 0 || fieldWidth <= right x landY

putEnemy :: Field RealWorld -> Enemy -> IO ()
putEnemy f (enemyX -> x) = fieldPutVariousHuman f looks x landY
	where looks = Human { humanHeadSize = LargeHead,
		humanLeftArm = UpArm, humanRightArm = UpArm }

enemyForward :: Enemy -> CInt -> Enemy
enemyForward (enemyX &&& enemyXCenti -> (x, xc)) dxc =
	Enemy { enemyX = x + d, enemyXCenti = m }
	where (d, m) = (xc - dxc) `divMod` 100

-- ENEMIES

type Enemies = [Enemy]

enemiesStep :: Point -> Enemies -> EnemyEnergy -> StdGen ->
	((Enemies, EnemyEnergy, Bool), StdGen)
enemiesStep p es ee g = ((maybe es' (: es') me, ee', isJust me), g'')
	where
	((me, ee'), g') = enemyEnergyAdd ee `first` randomR (0, enemyEnergy p) g
	(es', g'') = enemiesMove g' es

enemyEnergyAdd :: Int -> Int -> (Maybe Enemy, Int)
enemyEnergyAdd ee dee
	| ee + dee > 1000 = (Just $ Enemy enemyInitX 0, ee + dee - 1000)
	| otherwise = (Nothing, ee + dee)

enemyEnergy :: Int -> Int
enemyEnergy p
	| p < 60 = 20 + p `div` 10
	| p < 120 = 23 + p `div` 20
	| otherwise = 26 + p `div` 40

enemiesMove :: StdGen -> Enemies -> (Enemies, StdGen)
enemiesMove g = \case
	[] -> ([], g)
	e : es -> (enemyForward e dex :) `first` enemiesMove g' es
	where (dex, g') = randomR (- 10, 65) g

-- INPUT

data Input = Tick | Left | Stop | Right | Jump deriving Show

gameInput :: GameState -> Input -> GameState
gameInput g@GameState { gameStateHero = hr } = \case
	Tick -> gameTick g
	Left -> g { gameStateHero = heroLeft hr }
	Stop -> g { gameStateHero = hr { heroRun = Stand } }
	Right -> g { gameStateHero = heroRight hr }
	Jump -> g { gameStateHero = heroJump hr }

gameTick :: GameState -> GameState
gameTick gs@GameState {
	gameStateHero = (heroStep -> hr),
	gameStateEnemies = es, gameStateEnemyEnergy = ee,
	gameStateRandomGen = g, gameStatePoint = p } = gs {
	gameStateHero = hr,
	gameStateEnemies = filter (not . enemyOut) es'',
	gameStateEnemyEnergy = ee',
	gameStateRandomGen = g',
	gameStatePoint = p + 10 * length bs + bool 0 1 ne,
	gameStateFailure = checkOverlap hr `any` es'' }
	where
	((es', ee', ne), g') = enemiesStep p es ee g
	(es'', bs) = partition (not . checkBeat hr) es'

checkBeat, checkOverlap :: Hero -> Enemy -> Bool
checkBeat hr e = checkOverlap hr e && heroY hr < landY

checkOverlap (heroX &&& heroY -> (hrx, hry)) (Enemy ex _) =
	el <= hrr && hrl <= er && et <= hrb && hrt <= eb
	where
	[hrl, hrr, hrt, hrb] = (\f -> f hrx hry) <$> [left, right, top, bottom]
	[el, er, et, eb] = (\f -> f ex landY) <$> [left, right, top, bottom]
