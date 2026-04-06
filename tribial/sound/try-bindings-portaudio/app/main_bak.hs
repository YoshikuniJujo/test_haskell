{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Linear (V2(..))
import System.PortAudio
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as MV

period :: Int
period = 128

table :: V.Vector Float
table = V.fromList [ sin t |
	i <- [0 .. period - 1],
	let t = fromIntegral i / fromIntegral period * 2 * pi ]

callback :: TMVar Int -> Status -> input -> MV.IOVector (V2 Float) -> IO StreamCallbackResult
callback phase _ _ o = do
	i0 <- atomically $ takeTMVar phase
	go i0 0
	atomically $ putTMVar phase $ i0 + n
	pure Continue
	where
	n = MV.length o
	go :: Int -> Int -> IO ()
	go i0 i	| i == n = pure ()
		| otherwise = do
			let	v = table V.! ((i0 + i) `mod` period)
			MV.write o i (V2 v v)
			go i0 (i + 1)

main :: IO ()
main = withPortAudio do
	(_, dev0 : dev1 : dev2 : dev3 : dev4 : dev5 : dev6 : dev7 : devs) <- getDevices
	print dev0
	print dev1
	print dev2
	print dev3
	print dev4
	print dev5
	print dev6
	print dev7
	print devs
	phase <- atomically $ newTMVar 0
	let	output = streamParameters dev0 0
	withStream rate buf noConnection output mempty (callback phase)
		\s -> withStartStream s $ threadDelay $ 1000 * 1000
--		. const . threadDelay $ 1000 * 1000
	where
	rate = 48000
	buf = 512

