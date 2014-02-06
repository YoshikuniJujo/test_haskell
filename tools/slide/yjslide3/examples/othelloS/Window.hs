module Window (start, othello) where

import Game (
	Game, Turn(..), X(..), Y(..), Disk(..),
	turn, disks, initGame, nextGame)
import AI (aiN)
import Graphics.UI.WX (
	start, Prop(..), set, on,
	frameFixed, close, text, minsize, sz,
	layout, widget,
	Timer, timer, interval, command, enabled,
	Panel, panel, paint, click, charKey, repaint,
	DC, Rect, BrushKind(..),
	brushKind, brushColor, black, white, 
	Point, Point2(..), line, circle, drawText,
	Var, varCreate, varGet, varUpdate)

import Control.Arrow ((***))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.List (partition)

aiForesee :: Int
aiForesee = 3

aiWaitMs :: Int
aiWaitMs = 1000

leftMargin, rightMargin, topMargin, bottomMargin,
	squareSize, discRadius :: Int
leftMargin = 10
rightMargin = 50
topMargin = 10
bottomMargin = 20
squareSize = 30
discRadius = 12

msgLeft, charSpace :: Int
msgLeft = 80
charSpace = 20

boundRight, boundBottom, windowWidth, windowHeight :: Int
boundRight = leftMargin + squareSize * 8
boundBottom = topMargin + squareSize * 8
windowWidth = boundRight + rightMargin
windowHeight = boundBottom + bottomMargin + charSpace * 3

msgTop, msgTop2, msgLeft2 :: Int
msgTop = boundBottom + bottomMargin
msgTop2 = msgTop + charSpace
msgLeft2 = msgLeft + charSpace

othello :: IO ()
othello = do
	vg <- varCreate initGame
	f <- frameFixed [text := "othello"]
	t <- timer f [interval := aiWaitMs, enabled := False]
	p <- panel f [on (charKey 'q') := close f, on paint := paintGame vg]
	set t [on command := aiPlace vg p t]
	set p [on click := userPlace vg p t]
	set f [layout := minsize (sz windowWidth windowHeight) $ widget p]

paintGame :: Var Game -> DC a -> Rect -> IO ()
paintGame _ _ _ = return ()

aiPlace :: Var Game -> Panel () -> Timer -> IO ()
aiPlace _ _ _ = return ()

userPlace :: Var Game -> Panel () -> Timer -> Point -> IO ()
userPlace _ _ _ _ = return ()

paintLines :: DC a -> IO ()
paintLines dc = mapM_ lineV [0 .. 8] >> mapM_ lineH [0 .. 8]
	where
	cx x = x * squareSize + leftMargin
	cy y = y * squareSize + topMargin
	lineV x = line dc (Point (cx x) topMargin) (Point (cx x) boundBottom) []
	lineH y = line dc (Point leftMargin (cy y)) (Point boundRight (cy y)) []
