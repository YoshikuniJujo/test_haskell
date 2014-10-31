{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Bits
import Data.Typeable
import Data.IORef
import System.Random

initialPlayerHealth, initialPlayerAgility, initialPlayerStrength :: Int
initialPlayerHealth = 30
initialPlayerAgility = 30
initialPlayerStrength = 30

monsterNum :: Int
monsterNum = 12

class MonsterClass m where
	monsterShow :: m -> IO ()
	monsterAttack :: m -> Player -> IO ()
	monsterHealthRef :: m -> IORef Int
	monsterHit :: m -> Maybe (Int -> IO ())

instance MonsterClass Monster where
	monsterShow Monster{ monsterKind = mk } = monsterShow mk
	monsterAttack Monster{ monsterKind = mk } p = monsterAttack mk p
	monsterHealthRef Monster{ monsterKind = mk } = monsterHealthRef mk
	monsterHit m@Monster{ monsterKind = mk } = case monsterHit mk of
		Just mh -> Just mh
		_ -> Just $ \x -> do
			modifyIORef (monsterHealthRef mk) (subtract x)
			md <- monsterDead m
			if md
			then putStrLn $ "You killed the " ++
				show (typeOfMonster m) ++ "!"
			else putStrLn $ "You hit the " ++
				show (typeOfMonster m) ++ ", " ++
				"knocking off " ++ show x ++
				" health points!"

data Monster = forall m . (Typeable m, MonsterClass m) => Monster {
	monsterKind :: m
--	monsterHealthRef :: IORef Int
	}
data Player = Player {
	playerHealth :: IORef Int,
	playerAgility :: IORef Int,
	playerStrength :: IORef Int
	}
data GameResult = PlayerDead | MonstersDead deriving Show

orcBattle :: IO ()
orcBattle = do
	ms <- initMonsters
	p <- initPlayer
	r <- gameLoop p ms
	case r of
		PlayerDead -> putStrLn "You have been killed. Game Over."
		MonstersDead -> putStrLn $ "Congratulations! You have " ++
			"vanquished all of your foes."

initPlayer :: IO Player
initPlayer = do
	ht <- newIORef initialPlayerHealth
	ag <- newIORef initialPlayerAgility
	st <- newIORef initialPlayerStrength
	return $ Player {
		playerHealth = ht,
		playerAgility = ag,
		playerStrength = st
		}

playerDead :: Player -> IO Bool
playerDead = ((<= 0) <$>) . readIORef . playerHealth

gameLoop :: Player -> [Monster] -> IO GameResult
gameLoop p ms = do
	showPlayer p
	at <- succ . (`div` 15) . max 0 <$> readIORef (playerAgility p)
	replicateM_ at $ do
		msd <- monstersDead ms
		unless msd $ do
			showMonsters ms
			playerAttack p ms
	forM_ ms $ \m -> do
		md <- monsterDead m
		unless md $ monsterAttack m p
	pd <- playerDead p
	msd <- monstersDead ms
	case (pd, msd) of
		(True, _) -> return PlayerDead
		(_, True) -> return MonstersDead
		_ -> gameLoop p ms

showPlayer :: Player -> IO ()
showPlayer p = do
	ht <- readIORef $ playerHealth p
	ag <- readIORef $ playerAgility p
	st <- readIORef $ playerStrength p
	putStrLn $ "You are a valiant knight with a health of " ++
		show ht ++
		", an agility of " ++ show ag ++
		", and a strength of " ++ show st

showMonsters :: [Monster] -> IO ()
showMonsters ms = do
	putStrLn "Your foes:"
	(\proc -> zipWithM_ proc [1 :: Int ..] ms) $ \i m -> do
		putStr $ show i ++ ". "
		md <- monsterDead m
		if md then putStrLn "**dead**" else do
			mh <- monsterHealth m
			putStr $ "(Health=" ++ show mh ++ ") "
			monsterShow m

playerAttack :: Player -> [Monster] -> IO ()
playerAttack p ms = do
	st <- readIORef $ playerStrength p
	putStrLn "Attack style: [s]tab [d]ouble swing [r]oundhouse:"
	c <- getChar
	case c of
		's' -> do
			mid <- pickMonster ms
			r <- randomRIO (1, max 1 $ st `shiftR` 1)
			monsterHitById ms mid $ r + 2
		'd' -> do
			r <- randomRIO (1, max 1 $ st `div` 6)
			putStrLn $ "Your double swing has a strength of " ++
				show r
			mid1 <- pickMonster ms
			monsterHitById ms mid1 r
			msd <- monstersDead ms
			unless msd $ do
				mid2 <- pickMonster ms
				monsterHitById ms mid2 r
		_ -> do	t <- succ <$> randomRIO (1, max 1 $ st `div` 3)
			replicateM_ t $ do
				msd <- monstersDead ms
				unless msd $ do
					mid <- randomMonster ms
					monsterHitById ms mid 1

monsterHitById :: [Monster] -> Int -> Int -> IO ()
monsterHitById ms mid = case monsterHit $ ms !! (mid - 1) of
	Just mh -> mh
	_ -> error "bad"

pickMonster :: [Monster] -> IO Int
pickMonster ms = do
	putStr "Monster #: "
	ln <- getLine
	let n = read ln
	if all isDigit ln && 1 <= n && n <= monsterNum
	then do	md <- monsterDead $ ms !! (n - 1)
		if md
		then do	putStrLn ""
			pickMonster ms
		else return n
	else do	print "That is not a valid monster number."
		pickMonster ms

monstersDead :: [Monster] -> IO Bool
monstersDead = (and <$>) . mapM monsterDead

monsterDead :: Monster -> IO Bool
monsterDead = ((<= 0) <$>) . monsterHealth

monsterHealth :: Monster -> IO Int
monsterHealth = readIORef . monsterHealthRef

typeOfMonster :: Monster -> TypeRep
typeOfMonster Monster{ monsterKind = mk } = typeOf mk

randomMonster :: [Monster] -> IO Int
randomMonster ms = do
	mid <- randomRIO (1, monsterNum)
	md <- monsterDead $ ms !! (mid - 1)
	if md then randomMonster ms else return mid

initMonsters :: IO [Monster]
initMonsters = replicateM monsterNum $ do
	mk <- randomRIO (0, length monsterBuilders - 1)
	monsterBuilders !! mk

makeOrc :: IO Monster
makeOrc = do
	r <- randomRIO (1, 10)
	cl <- randomRIO (1, 8)
	mh <- newIORef r
	return Monster { monsterKind = Orc mh cl }

makeHydra :: IO Monster
makeHydra = do
	r <- randomRIO (1, 10)
	mh <- newIORef r
	return Monster { monsterKind = Hydra mh }

makeSlime :: IO Monster
makeSlime = do
	r <- randomRIO (1, 10)
	s <- randomRIO (1, 5)
	mh <- newIORef r
	return Monster { monsterKind = SlimeMold mh s }

makeBrigand :: IO Monster
makeBrigand = do
	r <- randomRIO (1, 10)
	mh <- newIORef r
	return Monster { monsterKind = Brigand mh }

monsterBuilders :: [IO Monster]
monsterBuilders = [makeOrc, makeHydra, makeSlime, makeBrigand]

data Orc = Orc {
	orcHealth :: IORef Int,
	clubLevel :: Int
	} deriving Typeable

instance MonsterClass Orc where
	monsterShow (Orc _ cl) = putStrLn $
		"A wicked orc with a level " ++ show cl ++ " club"
	monsterAttack (Orc _ cl) p = do
		x <- randomRIO (1, cl)
		putStrLn $ "An orc swings his club at you and knocks off " ++
			show x ++ " of your health points."
		modifyIORef (playerHealth p) $ subtract x
	monsterHealthRef = orcHealth
	monsterHit _ = Nothing

data Hydra = Hydra { hidraHealth :: IORef Int } deriving Typeable

instance MonsterClass Hydra where
	monsterShow h = do
		hh <- readIORef $ hidraHealth h
		putStrLn $ "A malicious hydra with " ++ show hh ++ " heads."
	monsterAttack h p = do
		hh <- readIORef $ hidraHealth h
		x <- randomRIO (1, max 1 $ hh `shiftR` 1)
		putStrLn $ "A hydra attacks you with " ++ show x ++
			" of its heads! It also grows back one more head!"
		modifyIORef (hidraHealth h) succ
		modifyIORef (playerHealth p) (subtract x)
	monsterHealthRef = hidraHealth
	monsterHit h = Just $ \x -> do
		modifyIORef (hidraHealth h) (subtract x)
		hh <- readIORef $ hidraHealth h
		if hh <= 0
		then putStrLn $ "The corpse of the fully decapitated " ++ 
				"and decapacitated hydra falls to the floor!"
		else putStrLn $ "You lop off " ++ show x ++ " of the hydra's heads!"

data SlimeMold = SlimeMold {
	slimeHealth :: IORef Int,
	slimeSliminess :: Int
	} deriving Typeable

instance MonsterClass SlimeMold where
	monsterShow s = putStrLn $
		"A slime mold with a sliminess of " ++ show (slimeSliminess s)
	monsterAttack s p = do
		x <- randomRIO (1, slimeSliminess s)
		putStrLn $ "A slime mold wraps around your legs " ++
			"and decreases your agility by " ++ show x ++ "!"
		modifyIORef (playerAgility p) (subtract x)
		a <- randomIO
		when a $ do
			putStrLn $ "It also squirts in your face, " ++
				"taking away a health point!"
			modifyIORef (playerHealth p) pred
	monsterHealthRef = slimeHealth
	monsterHit _ = Nothing

data Brigand = Brigand { brigandHealth :: IORef Int } deriving Typeable

instance MonsterClass Brigand where
	monsterShow _ = putStrLn "A fierce BRIGAND"
	monsterAttack _ p = do
		ph <- readIORef $ playerHealth p
		pa <- readIORef $ playerAgility p
		ps <- readIORef $ playerStrength p
		let x = maximum [ph, pa, ps]
		case (x == ph, x == pa, x == ps) of
			(True, _, _) -> do
				putStrLn $ "A brigand hits you with his " ++
					"slingshot, taking off 2 health points!"
				modifyIORef (playerHealth p) (subtract 2)
			(_, True, _) -> do
				putStrLn $ "A brigand catches your leg with his " ++
					"whip, taking off 2 agility points!"
				modifyIORef (playerAgility p) (subtract 2)
			(_, _, True) -> do
				putStrLn $ "A brigand cuts your arm with his " ++
					"whip, taking off 2 strength points!"
				modifyIORef (playerStrength p) (subtract 2)
			_ -> error "Can't occur!"
	monsterHealthRef = brigandHealth
	monsterHit _ = Nothing
