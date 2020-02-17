{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Maybe
import Data.Set
import Data.Time

import React
import MouseAndTime
import GuiEv

import Field

main :: IO ()
main = do
	f <- openField "スリープ" [exposureMask, buttonPressMask]
	t <- getCurrentTime
	(interpret (handle f) (sleep 3) `runStateT` t) >>= print
	closeField f

handle :: Field -> EvReqs GuiEv -> StateT UTCTime IO (EvOccs GuiEv)
handle f r = do
	t <- get
	n <- liftIO getCurrentTime
	put n
	if n `diffUTCTime` t >= time
	then do	put $ addUTCTime time t
		pure $ makeTimeObs r time
	else withNextEventTimeout f 500000 \case
		[] -> do
			liftIO $ print n
			pure . makeTimeObs r $ n `diffUTCTime` t
		es -> liftIO (putStrLn $ "event occur: " ++ show es) >> handle f r
	where time = getWait r

getWait :: Set GuiEv -> Time
getWait r = if Prelude.null al then big else minimum al where
	al = filterMap getTime (elems r)
	getTime (TryWait t _) = Just t
	getTime _ = Nothing

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (catMaybes .) . Prelude.map

big :: Time
big = 2 ^ (2 ^ (5 :: Int) :: Int)

makeTimeObs :: Set GuiEv -> Time -> Set GuiEv
makeTimeObs r t = fromList . filterMap makeOcc $ elems r where
	makeOcc (TryWait t' _) = Just $ TryWait t' (Occurred t)
	makeOcc (DeltaTime _) = Just $ DeltaTime (Occurred t)
	makeOcc _ = Nothing
