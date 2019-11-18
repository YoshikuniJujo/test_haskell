{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad (void, forever)
import Control.Monad.STM (STM, atomically, retry)
import Control.Monad.Tips (doWhile)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (
	TVar, newTVar, readTVar, writeTVar, modifyTVar )
import Data.RTQueue (RTQueue, empty, snoc, uncons, cons)
import System.Random (randomRIO)

data ZunDoko = Zun | Doko | Bye deriving Show
data Kiyoshi
	= Timeout | Pending
	| UntimelyDoko [ZunDoko] (RTQueue ZunDoko)
	| ZunOverflow (RTQueue ZunDoko)
	| Kiyoshi [ZunDoko]
	deriving Show
data Action =
	Retry | Exit [ZunDoko] String | Continue [ZunDoko] (RTQueue ZunDoko)
	deriving Show

say :: ZunDoko -> String
say = \case Zun -> "ズン"; Doko -> "ドコ"; Bye -> "<終了>"

main :: IO ()
main = do
	q <- atomically $ newTVar empty
	void . forkIO . forever $ ruffle >> atomically (enqueue q Zun)
	void . forkIO . forever $ ruffle >> atomically (enqueue q Doko)
	void . forkIO $ delay 50000 >> atomically (modifyTVar q $ cons Bye)
	(putStrLn =<<) . doWhile $ do
		(r, zds) <-
			atomically $ (action . check 0) <$> readTVar q >>= \case
				Retry -> retry
				Exit z msg -> pure (Just msg, z)
				Continue z q' -> (Nothing, z) <$ writeTVar q q'
		r <$ (putStrLn . say) `mapM_` zds

enqueue :: TVar (RTQueue a) -> a -> STM ()
enqueue q x = modifyTVar q (`snoc` x)

ruffle :: IO ()
ruffle = randomRIO (1, 1000) >>= delay

delay :: Int -> IO ()
delay = threadDelay . (* 100)

check :: Int -> RTQueue ZunDoko -> Kiyoshi
check n q = case uncons q of
	Nothing -> Pending
	Just (Bye, _) -> Timeout
	Just (Zun, q')
		| n < 4 -> case check (n + 1) q' of
			UntimelyDoko zds q'' -> UntimelyDoko (Zun : zds) q''
			ZunOverflow q'' -> ZunOverflow $ cons Zun q''
			Kiyoshi zds -> Kiyoshi (Zun : zds)
			k -> k
		| otherwise -> ZunOverflow q'
	Just (Doko, q')
		| n < 4 -> UntimelyDoko [Doko] q'
		| otherwise -> Kiyoshi [Doko]

action :: Kiyoshi -> Action
action Timeout = Exit [] "<タイムアウト>"
action Pending = Retry
action (UntimelyDoko zds q) = Continue zds q
action (ZunOverflow q) = Continue [Zun] q
action (Kiyoshi zds) = Exit zds "キ・ヨ・シ!"
