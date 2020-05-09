{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.TrySig (
	boxes,
	tryCycleColor, tryCurRect, tryElapsed, tryWiggleRect, tryCompleteRect,
	tryDefineRect, tryChooseBoxColor, tryChooseBoxColor', tryBoxes ) where

import Trials.Boxes (
	cycleColor, curRect, elapsed, wiggleRect,
	completeRect, defineRect, chooseBoxColor, chooseBoxColor', boxes )
import Trials.Boxes.Run
import Trials.Boxes.View

tryCycleColor :: IO ()
tryCycleColor = withInterpretSig "tryCycleColor" (const print) cycleColor

tryCurRect :: IO ()
tryCurRect = withInterpretSig "tryCurRect"
	(\f -> withFlush f . drawRect f 0xff0000) $ curRect (300, 200)

tryElapsed :: IO ()
tryElapsed = withInterpretSig "tryElapsed" drawElapsed elapsed

tryWiggleRect :: IO ()
tryWiggleRect = withInterpretSig "tryWiggleRect"
	(\f -> withFlush f . drawRect f 0xff0000)
	(wiggleRect $ Rect (200, 150) (400, 300))

tryCompleteRect :: IO ()
tryCompleteRect = withInterpretSig "tryCompleteRect"
	(\f -> withFlush f . drawRect f 0xff0000) (completeRect (200, 150))

tryDefineRect :: IO ()
tryDefineRect = withInterpretSig "tryDefineRect"
	(\f -> withFlush f . drawRect f 0xff0000) defineRect

tryChooseBoxColor :: IO ()
tryChooseBoxColor = withInterpretSig "tryChooseBoxColor"
	(\f -> withFlush f . drawBox f)
	(chooseBoxColor $ Rect (200, 150) (400, 300))

tryChooseBoxColor' :: IO ()
tryChooseBoxColor' = withInterpretSig "tryChooseBoxColor'"
	(\f -> withFlush f . drawBox f)
	(chooseBoxColor' $ Rect (200, 150) (400, 300))

tryBoxes :: IO ()
tryBoxes = withInterpretSig "tryBoxes"
	(\f -> withFlush f . (drawBox f `mapM_`) . reverse) boxes
