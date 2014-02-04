module Window (
	start,
	othello
) where

import Control.Arrow ((***))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.List (partition)

import AI
import Game
import Tools
import Graphics.UI.WX

aiWait :: Int
aiWait = 1000

aiRead :: Int
aiRead = 3

leftMargin, rightMargin, topMargin, bottomMargin, squareSize, msgLeft, charSize,
	discRadius :: Int
leftMargin = 10
rightMargin = 50
topMargin = 10
bottomMargin = 20
squareSize = 30
charSize = 20
msgLeft = 80
discRadius = 12

boundRight, boundBottom, windowWidth, windowHeight, msgTop, msgTop2, msgLeft2 :: Int
boundRight = leftMargin + squareSize * 8
boundBottom = topMargin + squareSize * 8
windowWidth = boundRight + rightMargin
windowHeight = boundBottom + bottomMargin + charSize * 3
msgTop = boundBottom + bottomMargin
msgTop2 = msgTop + charSize
msgLeft2 = msgLeft + charSize

othello :: IO ()
othello = do
	vgame <- varCreate initGame
	f <- frameFixed [text := "othello"]
	t <- timer f [interval := aiWait, enabled := False]
	p <- panel f [on (charKey 'q') := close f, on paint := paintBoard vgame]
	set t [on command := aiStone vgame p t]
	set p [on click := clickStone vgame p t]
	set f [layout := minsize (sz windowWidth windowHeight) $ widget p ]

paintBoard :: Var Game -> DC a -> Rect -> IO ()
paintBoard vgame dc _ = do
	game <- varGet vgame
	let	sts = stones game
		(b, w) = length *** length $ partition ((== Black) . snd) sts
	paintLines dc
	forM_ sts $ uncurry $ drawStone dc
	case turn game of
		Turn Black -> drawText dc "*" (Point msgLeft msgTop) []
		Turn White -> drawText dc "*" (Point msgLeft msgTop2) []
		_ -> return ()
	drawText dc ("Black: " ++ show b) (Point msgLeft2 msgTop) []
	drawText dc ("White: " ++ show w) (Point msgLeft2 msgTop2) []

paintLines :: DC a -> IO ()
paintLines dc = mapM_ lineV [0 .. 8] >> mapM_ lineH [0 .. 8]
	where
	cx x = x * squareSize + leftMargin
	cy y = y * squareSize + topMargin
	lineV x = line dc (Point (cx x) topMargin) (Point (cx x) boundBottom) []
	lineH y = line dc (Point leftMargin (cy y)) (Point boundRight (cy y)) []

drawStone :: DC a -> (X, Y) -> Stone -> IO ()
drawStone dc (x, y) s = do
	set dc [brushColor := stoneColor s, brushKind := BrushSolid]
	drawBall $ Point
		(fromEnum x * squareSize + squareSize `div` 2 + leftMargin)
		(fromEnum y * squareSize + squareSize `div` 2 + topMargin)
	where
	stoneColor Black = black
	stoneColor White = white
	drawBall p = circle dc p discRadius []

clickStone :: Var Game -> Panel () -> Timer -> Point -> IO ()
clickStone vgame p t (Point x y) = do
	_ <- varUpdate vgame $ \g -> fromMaybe g $ do
		x' <- maybeToEnum $ (x - leftMargin) `div` squareSize
		y' <- maybeToEnum $ (y - topMargin) `div` squareSize
		nextGame g (x', y')
	repaint p
	nextTurn vgame p t

aiStone :: Var Game -> Panel () -> Timer -> IO ()
aiStone vgame p t = do
	_ <- varUpdate vgame $ \g -> fromMaybe g $ do
		(pos, _) <- aiN aiRead g
		nextGame g pos
	repaint p
	nextTurn vgame p t

nextTurn :: Var Game -> Panel () -> Timer -> IO ()
nextTurn vgame p t = do
	g <- varGet vgame
	case turn g of
		Turn Black -> do
			set p [on click := clickStone vgame p t]
			set t [enabled := False]
		Turn White -> do
			set p [on click := const $ return ()]
			set t [enabled := True]
		_ -> do set p [on click := const $ return ()]
			set t [enabled := False]
