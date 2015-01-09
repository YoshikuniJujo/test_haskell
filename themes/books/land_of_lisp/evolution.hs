{-# LANGUAGE PackageImports #-}

import Prelude hiding (Left, Right)
import "monads-tf" Control.Monad.State
import Control.DeepSeq
import qualified Data.Set as Set
import Data.Bits
import Data.Char
import System.Random

width, height :: Int
width = 100
height = 30

jungle :: (Int, Int, Int, Int)
jungle = (45, 10, 10, 10)

plantEnergy, reproductionEnergy :: Int
plantEnergy = 80
reproductionEnergy = 200

type Plants = Set.Set (Int, Int)
type Animals = [Animal]
data Animal = Animal {
	animalPos :: (Int, Int),
	animalEnergy :: Int,
	animalDir :: Direction,
	animalGene :: Gene
	} deriving Show
data Direction = UpLeft | Up | UpRight | Right | DownRight | Down | DownLeft | Left
	deriving (Show, Enum)
data Gene = Gene {
	turnForward :: Int,
	turnForwardRight :: Int,
	turnRight :: Int,
	turnBackRight :: Int,
	turnBack :: Int,
	turnBackLeft :: Int,
	turnLeft :: Int,
	turnForwardLeft :: Int
	} deriving Show
type EvolutionStates = (StdGen, (Plants, Animals))
type EvolutionM = StateT EvolutionStates

instance NFData Animal where
	rnf a = animalPos a `seq` animalEnergy a `seq` animalDir a `seq` animalGene a `seq` ()

instance NFData StdGen where

isLeft, isRight, isUp, isDown :: Direction -> Bool
isLeft DownLeft = True; isLeft Left = True; isLeft UpLeft = True
isLeft _ = False
isRight UpRight = True; isRight Right = True; isRight DownRight = True
isRight _ = False
isUp UpLeft = True; isUp Up = True; isUp UpRight = True
isUp _ = False
isDown DownRight = True; isDown Down = True; isDown DownLeft = True
isDown _ = False

turnDegree :: Gene -> Int -> Int
turnDegree g 0 = turnForward g
turnDegree g 1 = turnForwardRight g
turnDegree g 2 = turnRight g
turnDegree g 3 = turnBackRight g
turnDegree g 4 = turnBack g
turnDegree g 5 = turnBackLeft g
turnDegree g 6 = turnLeft g
turnDegree g 7 = turnForwardLeft g
turnDegree _ _ = error "bad degree"

turnDenom :: Gene -> Int
turnDenom g = sum $ map (turnDegree g) [0 .. 7]

turnDirection :: Direction -> Int -> Direction
turnDirection dr d = toEnum $ (fromEnum dr + d) `mod` 8

turnDistribution :: Direction -> Gene -> [(Int, Direction)]
turnDistribution dr g = map (\n -> (turnDegree g n, turnDirection dr n)) [0 .. 7]

turnWithDistribution :: [(Int, Direction)] -> Int -> Direction
turnWithDistribution [] n = error $ "bad random value " ++ show n
turnWithDistribution ((n, d) : nds) r
	| r < n = d
	| otherwise = turnWithDistribution nds (r - n)

randomTurn :: Direction -> Gene -> StdGen -> (Direction, StdGen)
randomTurn dr gn sg = let
	(r, sg') = randomR (0, turnDenom gn - 1) sg
	td = turnDistribution dr gn in
	(turnWithDistribution td r, sg')

randomGene :: StdGen -> (Gene, StdGen)
randomGene sg = let
	(gn0, sg0) = randomR (1, 10) sg
	(gn1, sg1) = randomR (1, 10) sg0
	(gn2, sg2) = randomR (1, 10) sg1
	(gn3, sg3) = randomR (1, 10) sg2
	(gn4, sg4) = randomR (1, 10) sg3
	(gn5, sg5) = randomR (1, 10) sg4
	(gn6, sg6) = randomR (1, 10) sg5
	(gn7, sg7) = randomR (1, 10) sg6 in
	(Gene gn0 gn1 gn2 gn3 gn4 gn5 gn6 gn7, sg7)

initialize :: Monad m => EvolutionM m ()
initialize = do
	(sg, (_ps, _as)) <- get
	let (gn, sg') = randomGene sg
	put (sg', (Set.empty, [Animal {
		animalPos = (width `shiftR` 1, height `shiftR` 1),
		animalEnergy = 1000,
		animalDir = UpLeft,
		animalGene = gn }]))

randomPlant :: Monad m => Int -> Int -> Int -> Int -> EvolutionM m ()
randomPlant l t w h = do
	(sg, (ps, as)) <- get
	let	(dx, sg') = randomR (0, w - 1) sg
		(dy, sg'') = randomR (0, h - 1) sg'
	put (sg'', (Set.insert (l + dx, t + dy) ps, as))

addPlant :: Monad m => EvolutionM m ()
addPlant = do
	let (jl, jt, jw, jh) = jungle
	randomPlant jl jt jw jh
	randomPlant 0 0 width height

move :: Animal -> Animal
move (Animal (x, y) en dr gn) = let
	dx = case (isLeft dr, isRight dr) of
		(True, _) -> - 1
		(_, True) -> 1
		_ -> 0
	dy = case (isUp dr, isDown dr) of
		(True, _) -> - 1
		(_, True) -> 1
		_ -> 0 in
	Animal ((x + dx) `mod` width, (y + dy) `mod` height) (pred en) dr gn

turn :: Animal -> StdGen -> (Animal, StdGen)
turn (Animal pos en dr gn) sg = let (dr', sg') = randomTurn dr gn sg in
	(Animal pos en dr' gn, sg')

eat :: Animal -> Plants -> (Animal, Plants)
eat a@(Animal pos en dr gn) ps = if Set.member pos ps
	then (Animal pos (en + plantEnergy) dr gn, Set.delete pos ps)
	else (a, ps)

mapWithState :: (x -> s -> (x, s)) -> [x] -> s -> ([x], s)
mapWithState _ [] s = ([], s)
mapWithState f (x : xs) s = let
	(x', s') = f x s
	(xs', s'') = mapWithState f xs s' in
	(x' : xs', s'')

reproduce :: Animal -> StdGen -> ((Animal, Maybe Animal), StdGen)
reproduce a@(Animal pos en dr gn) sg
	| en >= reproductionEnergy = let
		(gn', sg') = randomMutate gn sg
		an = Animal pos (en `shiftR` 1) dr gn'
		in
		((Animal pos (en `shiftR` 1) dr gn, Just an), sg')
	| otherwise = ((a, Nothing), sg)

mapReproduce :: (x -> s -> ((x, Maybe x), s)) -> [x] -> s -> ([x], s)
mapReproduce _ [] s = ([], s)
mapReproduce f (x : xs) s = let
	((x', mc), s') = f x s
	(xs', s'') = mapReproduce f xs s' in
	(x' : (maybe id (:) mc xs'), s'')

randomMutate :: Gene -> StdGen -> (Gene, StdGen)
randomMutate gn sg = let
	(mp, sg') = randomR (0, 7) sg
	(md, sg'') = randomR (-1, 1) sg' in
	(mutate gn mp md, sg'')

mutate :: Gene -> Int -> Int -> Gene
mutate g 0 d = g { turnForward = max 1 $ turnForward g + d }
mutate g 1 d = g { turnForwardRight = max 1 $ turnForwardRight g + d }
mutate g 2 d = g { turnRight = max 1 $ turnRight g + d }
mutate g 3 d = g { turnBackRight = max 1 $ turnBackRight g + d }
mutate g 4 d = g { turnBack = max 1 $ turnBack g + d }
mutate g 5 d = g { turnBackLeft = max 1 $ turnBackLeft g + d }
mutate g 6 d = g { turnLeft = max 1 $ turnLeft g + d }
mutate g 7 d = g { turnForwardLeft = max 1 $ turnForwardLeft g + d }
mutate _ _ _ = error "bad"

updateWorld :: Monad m => EvolutionM m ()
updateWorld = do
	(sg0, (ps0, as0)) <- get
	let	as1 = filter ((> 0) . animalEnergy) as0
		(as2, sg1) = mapWithState turn as1 sg0
		as3 = map move as2
		(as4, ps1) = mapWithState eat as3 ps0
		(as5, sg2) = mapReproduce reproduce as4 sg1
	put $!! (sg2, (ps1, as5))
	addPlant

drawWorld :: EvolutionM IO ()
drawWorld = do
	(_, (ps, as)) <- get
	lift . putStr . unlines $ showWorld ps as

animalChar :: Animal -> Char
animalChar (Animal _ _ _ g)
	| sum (map (turnDegree g) [7, 0, 1]) * 4 > turnDenom g * 3 = 'L'
	| sum (map (turnDegree g) [3, 4, 5]) * 4 > turnDenom g * 3 = 'M'
	| sum (map (turnDegree g) [7, 0, 1]) * 2 > turnDenom g * 1 = 'T'
	| sum (map (turnDegree g) [3, 4, 5]) * 2 > turnDenom g * 1 = 'N'
	| sum (map (turnDegree g) [7, 0, 1]) * 7 > turnDenom g * 3 = 'C'
	| sum (map (turnDegree g) [3, 4, 5]) * 7 > turnDenom g * 3 = 'Q'
	| otherwise = 'O'

showWorld :: Plants -> Animals -> [String]
showWorld ps as = flip map [0 .. height - 1] $ \y ->
	('|' :) . (++ "|") .
	flip map [0 .. width - 1] $ \x ->
		case (filter (((x, y) ==) . animalPos) as, Set.member (x, y) ps) of
			(a : _, _) -> animalChar a
			(_, True) -> '*'
			_ -> ' '

evolution :: EvolutionM IO ()
evolution = do
	lift $ putStrLn ""
	drawWorld
	str <- lift getLine
	case str of
		"quit" -> return ()
		_ -> do	if not (null str) && all isDigit str
			then replicateWithDot (read str) updateWorld
			else updateWorld
			evolution

replicateWithDot :: MonadIO m => Int -> m a -> m ()
replicateWithDot n _ | n <= 0 = return ()
replicateWithDot n m
	| n `mod` 1000 == 0 =
		liftIO (putChar '.') >> m >> replicateWithDot (n - 1) m
	| otherwise = m >> replicateWithDot (n - 1) m

main :: IO ()
main = do
	sg <- getStdGen
	(initialize >> evolution) `evalStateT` (sg, (Set.empty, []))
