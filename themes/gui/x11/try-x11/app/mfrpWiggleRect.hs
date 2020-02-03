{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Set
import Data.Time
import System.Exit

import Field
import MonadicFrp

main :: IO ()
main = do
	f <- openField "ふるえる長方形" [exposureMask]
	ct <- getCurrentTime
	_ <- (`runStateT` ct)
		$ interpretSig (handle f) (liftIO . fillRect' f) . wiggleRect $ Rect (100, 50) (250, 150)
	closeField f

handle :: Field -> EvReqs GUIEv -> StateT UTCTime IO (EvOccs GUIEv)
handle f r = do
	t <- get
	n <- liftIO getCurrentTime
	put n
	withNextEventTimeout f 50000 \case
		Just (DestroyWindowEvent {}) -> liftIO $ closeField f >> exitSuccess
		Just (ExposeEvent {}) -> liftIO (flushField f) >> handle f r
		Just ev	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle f r
			| otherwise -> liftIO (print ev) >> handle f r
		Nothing -> pure . singleton . DeltaTime . Occurred . fromRational . toRational $ (n `diffUTCTime` t)

fillRect' :: Field -> Rect -> IO ()
fillRect' f (Rect (l, t) (r, u)) = do
	clearField f
	fillRect f 0xff0000 (round l) (round t) (round $ r - l) (round $ u - t)
	flushField f
