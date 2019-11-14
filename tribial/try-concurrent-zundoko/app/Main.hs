{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception.NeverOccur
import Data.RTQueue
import System.Random

data ZunDoko = Zun | Doko | Quit deriving Show

data Kiyoshi = Timeout | Pending | Kiyoshi | NotKiyoshi (RTQueue ZunDoko)
	deriving Show

main :: IO ()
main = do
	zdv <- atomically $ newTVar empty
	void $ forkIO . forever
		$ disturb >> putStrLn "ズン" >> atomically (enqueue zdv Zun)
	void $ forkIO . forever
		$ disturb >> putStrLn "ドコ" >> atomically (enqueue zdv Doko)
	void $ forkIO $ delay 50000 >> atomically (modifyTVar zdv $ cons Quit)
	k <- atomically . doWhile $ do
		zd <- readTVar zdv
		case kiyoshiCheck 0 zd of
			Timeout -> return $ Just False
			Pending -> retry
			Kiyoshi -> return $ Just True
			NotKiyoshi zd' -> writeTVar zdv zd' >> return Nothing
	putStrLn $ if k then "キ・ヨ・シ!" else "<タイムアウト>"

enqueue :: TVar (RTQueue a) -> a -> STM ()
enqueue tv x = modifyTVar tv (`snoc` x)

disturb :: IO ()
disturb = randomRIO (1, 1000) >>= delay

delay :: Int -> IO ()
delay = threadDelay . (* 100)

kiyoshiCheck :: Int -> RTQueue ZunDoko -> Kiyoshi
kiyoshiCheck n _ | n < 0 = neverOccur
kiyoshiCheck n q = case uncons q of
	Nothing -> Pending
	Just (Quit, _) -> Timeout
	Just (Zun, zd) -> kiyoshiCheck (n + 1) zd
	Just (Doko, zd)
		| n > 3 -> Kiyoshi
		| otherwise -> NotKiyoshi zd

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) return =<< act
