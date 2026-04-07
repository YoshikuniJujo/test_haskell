{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Concurrent.STM
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import System.PortAudio
import Graphics.UI.GLFW qualified as Glfw
import Linear (V2(..))
import KeyEvent
import PlaySound
import Hz qualified as Hz

main :: IO ()
main = do
	end <- atomically $ newTVar False
	la <- atomically $ newTVar Mu
	vkas <- withKeyActions 100 100 "Hello"
	playSound 0 48000 \(o :: MV.IOVector (V2 Float)) -> do
		kas <- atomically
			$ readTVar vkas <* writeTVar vkas []
		print kas
		atomically $ writeTVar la Mu
		for_ kas \k -> do
			atomically . when (keyActionKey k == Glfw.Key'Q) $ writeTVar end True
			atomically . when (keyActionKey k == Glfw.Key'K) $ writeTVar la La
			atomically . when (keyActionKey k == Glfw.Key'D) $ writeTVar la Do
			atomically . when (keyActionKey k == Glfw.Key'F) $ writeTVar la Re
			atomically . when (keyActionKey k == Glfw.Key'G) $ writeTVar la Mi
		b <- atomically $ readTVar la
		print b
		if (b /= Mu)
		then do
			let	wf = case b of
					Do -> waveformDo
					Re -> waveformRe
					Mi -> waveformMi
					La -> waveformLa
				prd = round $ Hz.period case b of
					Do -> Hz.doo
					Re -> Hz.re
					Mi -> Hz.mi
					La -> Hz.la
			for_ [0 .. MV.length o - 1] \i -> do
				let	v = wf V.! (i `mod` prd)
					v' = v * size i
				MV.write o i $ V2 v' v'
		else for_ [0 .. MV.length o - 1] \i -> MV.write o i $ V2 0 0
		e <- atomically $ readTVar end
		pure $ if e then Complete else Continue
	where

size :: Int -> Float
size i	| i < 12000 = 0.7 * fromIntegral i / 12000
	| i < 36000 = 0.7
	| i < 48000 = 0.7 * (48000 - fromIntegral i) / 12000
	| otherwise = 0

data Doremi
	= Mu
	| Do | Re | Mi | Fa | So | La | Ti | HDo deriving (Show, Eq)

waveformDoremi :: Doremi -> V.Vector Float
waveformDoremi = \case
	Do -> waveformDo
	La -> waveformLa

waveformDo, waveformRe, waveformMi, waveformLa :: V.Vector Float
waveformDo = Hz.waveform Hz.doo
waveformRe = Hz.waveform Hz.re
waveformMi = Hz.waveform Hz.mi
waveformLa = Hz.waveform Hz.la
