{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import System.PortAudio
import Linear (V2(..))
import Options.Applicative

import Hz qualified as Hz

period :: Int
period = 128

samplingRate :: Double
samplingRate = 48000

table' :: Double -> V.Vector Float
table' hz = V.fromList [ sin $ realToFrac t |
	i <- [0 .. period - 1],
	let	t = i * hz / samplingRate * 2 * pi ]
	where
	period = samplingRate / hz

table :: V.Vector Float
table = V.fromList [ sin t |
	i <- [0..period - 1],
	let t = fromIntegral i / fromIntegral period * 2 * pi ]

callback :: TMVar Int ->
	Status -> input -> MV.IOVector (V2 Float) -> IO StreamCallbackResult
callback phase _ _ o = do
	i0 <- atomically do
		i <- takeTMVar phase
		putTMVar phase $ i + n
		pure i
	go i0 0
	pure Continue
	where
	n = MV.length o
	go :: Int -> Int -> IO ()
	go i0 i
		| i == n = return ()
		| otherwise = do
		let	v = table' Hz.la V.! ((i0 + i) `mod` round (samplingRate / Hz.la))
			sz = size (i0 + i)
			v' = v * sz
		MV.write o i (V2 v' v')
--		MV.write o i (V2 (v * 1) (v * 1))
		go i0 (i + 1)

size :: Int -> Float
size n	| n < 1000 = fromIntegral n / 2000
	| n < 100000 = 0.5
	| n < 261000 = 0.5 - fromIntegral (n - 101000) / 320000
	| n < 262000 = fromIntegral (n - 261000) / 2000
	| n < 362000 = 0.5
	| n < 370000 = 0.5 - fromIntegral (n - 362000) / 16000
	| otherwise = 0

-- size = [0, 0.02 .. 0.5] ++ [0.5, 0.49990 .. 0] ++ [0 ..]
-- size = [0, 0.02 .. 0.5] ++ replicate 4000 0.5 ++ [0.5, 0.499 .. 0] ++ [0 ..]
-- size = repeat 1

main :: IO ()
main = join $ execParser (info app mempty)

app :: Parser (IO ())
app = do
	rate <- option auto $ long "rate" <> help "sampling rate" <> value samplingRate
	buf <- option auto $ long "buffer" <> help "number of samples for the buffer" <> value 1024
	device <- option auto $ long "device" <> help "device index" <> value (-1)
	helper
	pure $ withPortAudio $ do
		(_, devs) <- getDevices
		if device < 0
		then forM_ (zip [0 :: Int ..] devs) $ \(i, dev) ->
			putStrLn $ show i ++ ": " ++ deviceName dev
		else do
			let dev = devs !! device
			phase <- atomically $ newTMVar 0
			let output = streamParameters dev 0
			withStream rate buf noConnection output mempty (callback phase) $ \s -> do
				setStreamFinishedCallback s $ putStrLn "Done"
				withStartStream s $ threadDelay $ 16 * 1000 * 1000
