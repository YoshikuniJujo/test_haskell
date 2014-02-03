module Window (
	start,
	othello
) where

import Data.Maybe

import AI
import Game
import Graphics.UI.WX

othello :: IO ()
othello = do
	vgame <- varCreate initGame
	f <- frameFixed [text := "othello"]
	t <- timer f []
	p <- panel f []
	set p [
		on click := clickStone p vgame t,
		on paint := paintBoard vgame,
		on (charKey 'q') := close f ]
	set t [
		interval := 1000,
		on command := aiStone vgame p t,
		enabled := False ]
	set f [layout := minsize (sz 300 300) $ widget p]

paintBoard :: Var Game -> DC a -> Rect -> IO ()
paintBoard vgame dc _ = do
	game <- varGet vgame
	mapM_ (\(p, s) -> drawStone s p dc) $ stones game
	paintLines dc
	let	b = length $ filter ((== Black) . snd) $ stones game
		w = length $ filter ((== White) . snd) $ stones game
	case turn game of
		Turn Black -> drawText dc "*" (Point 80 230) []
		Turn White -> drawText dc "*" (Point 80 250) []
		_ -> return ()
	drawText dc ("Black: " ++ show b) (Point 100 230) []
	drawText dc ("White: " ++ show w) (Point 100 250) []

paintLines :: DC a -> IO ()
paintLines dc = do
	mapM_ lineV [0 .. 8]
	mapM_ lineH [0 .. 8]
	where
	lineV x = line dc (Point (x * 25 + 7) 7) (Point (x * 25 + 7) (8 * 25 + 7)) []
	lineH y = line dc (Point 7 (y * 25 + 7)) (Point (8 * 25 + 7) (y * 25 + 7)) []

drawStone :: Stone -> (X, Y) -> DC a -> IO ()
drawStone s (x, y) dc = do
	set dc [brushColor := stoneColor s, brushKind := BrushSolid]
	drawBall dc (Point (fromEnum x * 25 + 20) (fromEnum y * 25 + 20))
	where
	stoneColor Black = black
	stoneColor White = white

drawBall :: DC a -> Point -> IO ()
drawBall dc p = circle dc p 10 []

maybeToXY :: Enum a => Int -> Maybe a
maybeToXY n
	| n < 0 || n > 7 = Nothing
	| otherwise = Just $ toEnum n

clickStone :: Panel () -> Var Game -> Timer -> Point -> IO ()
clickStone p vgame t (Point x y) = do
	_ <- varUpdate vgame $ \g -> fromMaybe g $ do
		x' <- maybeToXY $ (x - 10) `div` 25
		y' <- maybeToXY $ (y - 10) `div` 25
		nextGame g (x', y')
	repaint p
	nextTurn vgame p t

aiStone :: Var Game -> Panel () -> Timer -> IO ()
aiStone vgame p t = do
	_ <- varUpdate vgame $ \g -> fromMaybe g $ do
		(pos, _) <- aiN 3 g
		nextGame g pos
	repaint p
	nextTurn vgame p t

nextTurn :: Var Game -> Panel () -> Timer -> IO ()
nextTurn vgame p t = do
	g <- varGet vgame
	case turn g of
		Turn Black -> do
			set p [on click := clickStone p vgame t]
			set t [enabled := False]
		Turn White -> do
			set p [on click := const $ return ()]
			set t [enabled := True]
		_ -> do set p [on click := const $ return ()]
			set t [enabled := False]
