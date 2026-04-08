{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Concurrent.STM
import Data.Foldable
import Data.Maybe
import Data.Vector.Storable.Mutable qualified as MV
import System.PortAudio
import Graphics.UI.GLFW qualified as Glfw
import Linear (V2(..))
import KeyEvent
import PlaySound
import Sound
import Doremi
-- import Loudness

main :: IO ()
main = do
	time <- atomically $ newTVar 0
	sd <- atomically $ newTVar zeroSound
	print =<< Glfw.getTime
	vkas <- withKeyActions 100 100 "Hello"
--	playSound 0 48000 \(o :: MV.IOVector (V2 Float)) -> do
	playSound 0 4800 \(o :: MV.IOVector (V2 Float)) -> do
		mtm <- Glfw.getTime
		tm0 <- atomically do
			t <- readTVar time
			maybe (pure ()) (writeTVar time) mtm
			pure t
		print tm0
		kas <- atomically $ readTVar vkas <* writeTVar vkas []
		print kas
		s <- atomically $ readTVar sd
		let	evs = keyActionToChanger tm0 `mapMaybe` kas
			(wf, s') = sound s (MV.length o) (MV.length o) evs

		print evs
		for_ ([0 ..] `zip` wf) \(i, v) -> do
			when (v < - 1 || v > 1) $ putStrLn "OVER"
			MV.write o i (V2 v v)
		atomically $ writeTVar sd s'
		print s'
		print $ length wf

		pure if endKey kas then Complete else Continue

endKey :: [KeyAction] -> Bool
endKey = not . null . filter ((== Glfw.Key'Q) . keyActionKey)

keyActionToChanger :: Double -> KeyAction -> Maybe (Int, (Doremi, Float, Float))
keyActionToChanger tm0 KeyAction {
	keyActionKey = k, keyActionTime = tm, keyActionAction = pr } = let
	n = (tm - tm0) * 48000
	(d, t) = case pr of
		Press -> (1 / 1000, 0.7)
		Release -> (- 1 / 36000, 0) in (\nt -> (round n, (nt, d, t))) <$> keyToDoremi k

keyToDoremi :: Glfw.Key -> Maybe Doremi
keyToDoremi = (`lookup` keyDoremiTable)

keyDoremiTable :: [(Glfw.Key, Doremi)]
keyDoremiTable =
	[	Glfw.Key'A, Glfw.Key'S, Glfw.Key'D, Glfw.Key'F, Glfw.Key'G,
		Glfw.Key'H, Glfw.Key'J, Glfw.Key'K, Glfw.Key'L, Glfw.Key'Semicolon,
		Glfw.Key'Apostrophe
		] `zip`
	[LLa, LTi, Do, Re, Mi, Fa, So, La, Ti, HDo, HRe]
