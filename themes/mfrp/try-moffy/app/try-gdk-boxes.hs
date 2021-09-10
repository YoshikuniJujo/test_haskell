{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (repeat)

import Control.Moffy
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Viewable.Shape
import System.Environment
import System.Console.GetOpt

import Trial.TryGdk.Boxes
import Trial.Boxes
import Trial.Paper

main :: IO ()
main = do
	(os, _as, ems) <- getOpt Permute [
		optColor, optMousePos, optCurRect, optWiggleRect, optPosInside,
		optFirstPoint, optCompleteRect, optDefineRect,
		optChooseBoxColor, optBox
		] <$> getArgs
	putStrLn `mapM_` ems
	case os of
		[OptColor] -> tryGdk showColor $ adjustSig cycleColor
		[OptMousePos] -> tryGdk @_ @() (const print) $ adjustSig mousePos
		[OptCurRect] -> tryGdk @_ @() showRect . adjustSig $ curRect (100, 100)
		[OptWiggleRect] -> tryGdk @_ @() showRect
			. adjustSig . wiggleRect $ Rect (300, 200) (600, 400)
		[OptPosInside] -> tryGdk @_ @() (const print) . adjustSig
			. repeat $ posInside
				(Rect (300, 200) (600, 400)) mousePos
		[OptFirstPoint] -> tryGdk @_ @() (const print) . adjustSig
			$ repeat firstPoint
		[OptCompleteRect] -> tryGdk showRect . adjustSig
			$ completeRect (300, 200)
		[OptDefineRect] -> tryGdk showRect $ adjustSig defineRect 
		[OptChooseBoxColor] -> tryGdk showBox . adjustSig
			. chooseBoxColor $ Rect (300, 200) (600, 400)
		[OptBox] -> tryGdk showBox $ adjustSig box
		[] -> tryGdk showBoxes boxes
		_ -> putStrLn "Bad options"

data OptSetting
	= OptColor | OptMousePos | OptCurRect | OptWiggleRect | OptPosInside
	| OptFirstPoint | OptCompleteRect | OptDefineRect | OptChooseBoxColor
	| OptBox
	deriving (Show, Eq)

optColor, optMousePos, optCurRect, optWiggleRect, optPosInside, optFirstPoint,
	optCompleteRect, optDefineRect, optChooseBoxColor, optBox :: OptDescr OptSetting
optColor = Option ['c'] ["color"] (NoArg OptColor) "Cycle color"

optMousePos = Option ['m'] ["mouse-pos"] (NoArg OptMousePos) "Mouse position"

optCurRect = Option ['r'] ["cur-rect"] (NoArg OptCurRect) "Cur rect"

optWiggleRect = Option ['w'] ["wiggle-rect"] (NoArg OptWiggleRect) "Wiggle rect"

optPosInside = Option ['p'] ["pos-inside"] (NoArg OptPosInside) "Pos inside"

optFirstPoint = Option ['f'] ["first-point"] (NoArg OptFirstPoint) "First point"

optCompleteRect =
	Option [] ["complete-rect"] (NoArg OptCompleteRect) "Complete rect"

optDefineRect = Option ['d'] ["define-rect"] (NoArg OptDefineRect) "Define rect"

optChooseBoxColor = Option
	[] ["choose-box-color"] (NoArg OptChooseBoxColor) "Choose box color"

optBox = Option ['b'] ["box"] (NoArg OptBox) "Box"
