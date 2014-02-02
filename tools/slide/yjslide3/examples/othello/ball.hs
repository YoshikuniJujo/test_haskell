module Main where

import Graphics.UI.WX
import Othello
import Control.Concurrent

radius, maxX, maxY :: Int
maxY = 300
maxX = 300
radius = 10

maxH :: Int
maxH = maxY - radius

main :: IO ()
main = start ballsFrame

ballsFrame = do
	game <- varCreate initGame
	vballs <- varCreate []
	f <- frameFixed [text := "Bouncing balls"]
	p <- panel f [on paint := paintGame game]
	t <- timer f []
	set t [
		interval := 1000,
		on command := aiStone game p >> set t [enabled :~ not],
		enabled := False] -- nextBalls vballs p]

	set p [
		on click := clickStone game p t,
		on (charKey 'q') := close f
	 ]
	set f [layout := minsize (sz maxX maxY) $ widget p]

paintGame :: Var Game -> DC a -> Rect -> IO ()
paintGame vgame dc viewArea = do
	game <- varGet vgame
	set dc [brushColor := black, brushKind := BrushSolid]
	mapM_ (\(p, s) -> drawStone s p dc) $ addIndex $ board game
	line dc (Point 7 7) (Point (8 * 25 + 7) 7) []
	line dc (Point 7 7) (Point 7 (8 * 25 + 7)) []
	{-
	drawBall dc (Point 100 100)
	line dc (Point 200 200) (Point 100 100) []
	-}

addIndex :: [[a]] -> [((Int, Int), a)]
addIndex xss = concatMap (\(y, xs) -> zip (zip [0 ..] [y, y ..]) xs) $ zip [0 ..] xss

drawStone :: Stone -> (Int, Int) -> DC a -> IO ()
drawStone s (x, y) dc = do
	if s == Empty then return () else do
		set dc [brushColor := stoneColor s, brushKind := BrushSolid]
		drawBall dc (Point (x * 25 + 20) (y * 25 + 20))
	line dc	(Point ((x + 1) * 25 + 7) ( y      * 25 + 7))
		(Point ((x + 1) * 25 + 7) ((y + 1) * 25 + 7)) []
	line dc	(Point ( x      * 25 + 7) ((y + 1) * 25 + 7))
		(Point ((x + 1) * 25 + 7) ((y + 1) * 25 + 7)) []
	where
	stoneColor Black = black
	stoneColor White = white

clickStone :: Var Game -> Panel () -> Timer -> Point -> IO ()
clickStone game p t (Point x y) = do
	g <- varGet game
	varUpdate game (nextGameIf ((x - 10) `div` 25, (y - 10) `div` 25))
	case nextGame g ((x - 10) `div` 25, (y - 10) `div` 25) of
		Nothing -> return ()
		Just g' | turn g' == Black -> return ()
		_ -> set t [enabled := True]
	repaint p
--	repaint p
--	threadDelay 1000000
--	varUpdate game (\g -> nextGameIf (ai g) g)
--	repaint p
	return ()

aiStone :: Var Game -> Panel () -> IO ()
aiStone game p = do
	varUpdate game (\g -> nextGameIf (aiN 3 g) g) >> repaint p
	g <- varGet game
	if noSpace g
		then do	varUpdate game (\g -> nextGameIf (aiN 3 g) g) >> repaint p
			aiStone game p
		else return ()

nextGameIf :: (Int, Int) -> Game -> Game
nextGameIf pos g = case nextGame g pos of
	Just g' -> g'
	_ -> g

paintBalls :: Var [[Point]] -> DC a -> Rect -> IO ()
paintBalls vballs dc viewArea = do
	balls <- varGet vballs
	set dc [brushColor := red, brushKind := BrushSolid]
	mapM_ (drawBall dc) [p | (p : ps) <- balls]

drawBall dc pt = circle dc pt radius []

nextBalls :: Var [[Point]] -> Panel () -> IO ()
nextBalls vballs p = do
	varUpdate vballs (filter (not . null) . map (drop 1))
	repaint p

dropBall :: Var [[Point]] -> Panel () -> Point -> IO ()
dropBall vballs p pt = do
	varUpdate vballs (bouncing pt:)
	repaint p

bouncing (Point x y) =
	map (\h -> Point x (maxH - h)) (bounce (maxH - y) 0)

bounce h v
	| h <= 0 && v == 0 = replicate 20 0
	| h <= 0 && v < 0 = bounce 0 ((- v) - 2)
	| otherwise = h : bounce (h + v) (v - 1)
