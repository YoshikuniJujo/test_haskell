{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBox (tryBoxes) where

import Prelude hiding (cycle, repeat, scanl, until, break)
import qualified Prelude as P

import Control.Moffy
import Control.Moffy.Run
import Data.Time.Clock.System

import Control.Moffy.Event.Delete
import Field hiding (Point)

import Trial.Boxes

import Trial.Boxes.Event
import Trial.Boxes.Handle

withFlush :: Field -> IO a -> IO a
withFlush f act = clearField f >> act <* flushField f

drawRect :: Field -> Pixel -> Rect -> IO ()
drawRect f clr (Rect (l_, u_) (r_, d_)) = fillRect f clr l u w h where
	[l, u] = fromIntegral <$> [l_ `min` r_, u_ `min` d_]
	[w, h] = fromIntegral <$> [abs $ r_ - l_, abs $ d_ - u_]


colorToPixel :: Color -> Pixel
colorToPixel = \case
	Red -> 0xff0000; Green -> 0x00ff00; Blue -> 0x0000ff
	Yellow -> 0xffff00; Cyan -> 0xff00ff; Magenta -> 0x00ffff

drawBox :: Field -> Box -> IO ()
drawBox f (Box rct clr) = drawRect f (colorToPixel clr) rct

trySigGBoxes' :: String -> SigG s [Box] r -> IO r
trySigGBoxes' ttl sig = do
	f <- openField ttl [
		pointerMotionMask, buttonPressMask, buttonReleaseMask,
		exposureMask ]
	(r, _) <- interpretSt (handleBoxes 0.05 f) (drawBoxes f) sig . (InitialMode ,)
			. systemToTAITime =<< getSystemTime
	r <$ closeField f

drawBoxes :: Field -> [Box] -> IO ()
drawBoxes f = withFlush f . (drawBox f `mapM_`) . P.reverse

tryBoxes :: IO ()
tryBoxes = () <$ trySigGBoxes' "TRY BOXES" (boxes `break` deleteEvent)
