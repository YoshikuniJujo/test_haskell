{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad (void, forever)
import Control.Monad.STM (STM, atomically, retry)
import Control.Monad.Tips (doWhile)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (
	TVar, newTVar, readTVar, writeTVar, modifyTVar )
import Data.Bool (bool)
import Data.RTQueue (RTQueue, empty, snoc, uncons, cons)
import System.Random (randomRIO)

data ZunDoko = Zun | Doko | Bye deriving Show
data Kiyoshi
	= Timeout | Pending
	| Kiyoshi [ZunDoko] | NotKiyoshi [ZunDoko] (RTQueue ZunDoko)
	| ZunOverflow ZunDoko (RTQueue ZunDoko)
	deriving Show

say :: ZunDoko -> String
say = \case Zun -> "ズン"; Doko -> "ドコ"; Bye -> "<終了>"

main :: IO ()
main = do
	q <- atomically $ newTVar empty
	void . forkIO . forever $ ruffle >> atomically (enqueue q Zun)
	void . forkIO . forever $ ruffle >> atomically (enqueue q Doko)
	void . forkIO $ delay 50000 >> atomically (modifyTVar q $ cons Bye)
	(putStrLn . bool "<タイムアウト>" "キ・ヨ・シ!" =<<) . doWhile $ do
		r <- atomically $ kiyoshiCheck 0 <$> readTVar q >>= \case
			Timeout -> pure Nothing
			Pending -> retry
			NotKiyoshi zds q' -> Just (False, zds) <$ writeTVar q q'
			ZunOverflow zun q' -> Just (False, [zun]) <$ writeTVar q q'
			Kiyoshi zds -> pure $ Just (True, zds)
		($ r) . maybe (pure $ Just False) $ \(k, z) ->
			bool Nothing (Just True) k <$ (putStrLn . say) `mapM_` z

enqueue :: TVar (RTQueue a) -> a -> STM ()
enqueue q x = modifyTVar q (`snoc` x)

ruffle :: IO ()
ruffle = randomRIO (1, 1000) >>= delay

delay :: Int -> IO ()
delay = threadDelay . (* 100)

kiyoshiCheck :: Int -> RTQueue ZunDoko -> Kiyoshi
kiyoshiCheck n q = case uncons q of
	Nothing -> Pending
	Just (Bye, _) -> Timeout
	Just (Zun, zd)
		| n < 4 -> case kiyoshiCheck (n + 1) zd of
			NotKiyoshi zds zd' -> NotKiyoshi (Zun : zds) zd'
			Kiyoshi zds -> Kiyoshi (Zun : zds)
			ZunOverflow zun zd' -> ZunOverflow Zun (cons zun zd')
			k -> k
		| otherwise -> ZunOverflow Zun zd
	Just (Doko, zd)
		| n < 4 -> NotKiyoshi [Doko] zd
		| otherwise -> Kiyoshi [Doko]
