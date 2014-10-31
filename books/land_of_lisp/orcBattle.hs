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

instance MonsterClass Monster where
	monsterShow Monster{ monsterKind = mk } = monsterShow mk
	monsterAttack Monster{ monsterKind = mk } p = monsterAttack mk p

data Monster = forall m . (Typeable m, MonsterClass m) => Monster {
	monsterKind :: m,
	monsterHealthRef :: IORef Int
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
			monsterHit ms mid $ r + 2
		'd' -> do
			r <- randomRIO (1, max 1 $ st `div` 6)
			putStrLn $ "Your double swing has a strength of " ++
				show r
			mid1 <- pickMonster ms
			monsterHit ms mid1 r
			msd <- monstersDead ms
			unless msd $ do
				mid2 <- pickMonster ms
				monsterHit ms mid2 r
		_ -> do	t <- succ <$> randomRIO (1, max 1 $ st `div` 3)
			replicateM_ t $ do
				msd <- monstersDead ms
				unless msd $ do
					mid <- randomMonster ms
					monsterHit ms mid 1
	putStrLn "player atack monsters"

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

monsterHit :: [Monster] -> Int -> Int -> IO ()
monsterHit ms mid x = do
	modifyIORef (monsterHealthRef m) (subtract x)
	md <- monsterDead m
	if md
	then putStrLn $ "You killed the " ++
		show (typeOfMonster m) ++ "!"
	else putStrLn $ "You hit the " ++
		show (typeOfMonster m) ++ ", " ++
		"knocking off " ++ show x ++ " health points!"
	where
	m = ms !! (mid - 1)

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

makeMonster :: IO Monster
makeMonster = do
	r <- randomRIO (1, 10)
	cl <- randomRIO (1, 8)
	mh <- newIORef r
	return Monster {
		monsterKind = Orc cl,
		monsterHealthRef = mh
		}

makeOrc :: IO Monster
makeOrc = do
	r <- randomRIO (1, 10)
	cl <- randomRIO (1, 8)
	mh <- newIORef r
	return Monster {
		monsterKind = Orc cl,
		monsterHealthRef = mh
		}

monsterBuilders :: [IO Monster]
monsterBuilders = [makeOrc]

data Orc = Orc {
	clubLevel :: Int
	} deriving (Show, Typeable)

instance MonsterClass Orc where
	monsterShow (Orc cl) = putStrLn $
		"A wicked orc with a level " ++ show cl ++ " club"
	monsterAttack (Orc cl) p = do
		x <- randomRIO (1, cl)
		putStrLn $ "An orc swings his club at you and knocks off " ++
			show x ++ " of your health points."
		modifyIORef (playerHealth p) $ subtract x
