{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import System.PortAudio
import Linear (V2(..))
import Options.Applicative

import Hz qualified as Hz
import PlaySound

main :: IO ()
main = join $ execParser (info app mempty)

app :: Parser (IO ())
app = do
	buf <- option auto $ long "buffer" <>
		help "number of samples for the buffer" <> value 1024
	device <- option auto $ long "device" <>
		help "device index" <> value (-1)
	helper
	pure $ run device buf

run :: Int -> Int -> IO ()
run device buf = do
	phase <- atomically $ newTMVar 0
	playSound device buf \o -> do
		i0 <- atomically
			$ (<$) <$> id <*> putTMVar phase . (+ MV.length o)
				=<< takeTMVar phase
		($ 0) $ fix \go i -> if i == MV.length o then pure () else do
			let	v = Hz.waveform Hz.la V.! ((i0 + i) `mod` la)
				v' = v * size (i0 + i)
			MV.write o i (V2 v' v')
			go $ i + 1
		pure if (i0 > 370000) then Complete else Continue
	where
	la = round $ Hz.samplingRate / Hz.la

size :: Int -> Float
size n	| n < 1000 = fromIntegral n / 2000
	| n < 100000 = 0.5
	| n < 261000 = 0.5 - fromIntegral (n - 101000) / 320000
	| n < 262000 = fromIntegral (n - 261000) / 2000
	| n < 362000 = 0.5
	| n < 370000 = 0.5 - fromIntegral (n - 362000) / 16000
	| otherwise = 0
