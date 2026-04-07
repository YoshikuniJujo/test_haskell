{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PlaySound (playSound) where

import Control.Concurrent.STM
import Control.Monad
import Data.Vector.Storable.Mutable qualified as MV
import System.PortAudio

import Hz qualified as Hz

playSound :: (
	Applicative f, Foldable f, MV.Storable (f a), PortAudioSample a ) =>
	Int -> Int -> Callback (MV.IOVector (f a)) -> IO ()
playSound device buf cb = do
		end <- atomically newEmptyTMVar
		withPortAudio $ do
			(_, devs) <- getDevices
			if device < 0
			then forM_ (zip [0 :: Int ..] devs) $ \(i, dev) ->
				putStrLn $ show i ++ ": " ++ deviceName dev
			else do
				let dev = devs !! device
				withStream' Hz.samplingRate buf dev cb $ \s -> do
					setStreamFinishedCallback s do
						putStrLn "Done"
						atomically $ putTMVar end ()
					withStartStream s do -- $ threadDelay $ 16 * 1000 * 1000
						atomically $ takeTMVar end

withStream' ::
	(Applicative f, Foldable f, MV.Storable (f a), PortAudioSample a) =>
	Double -> Int -> Device Output -> Callback (MV.IOVector (f a)) ->
	(Stream -> IO r) -> IO r
withStream' rt bf dv cb = withStream
	rt bf noConnection (streamParameters dv 0) mempty (const $ const cb)

type Callback o = o -> IO StreamCallbackResult
