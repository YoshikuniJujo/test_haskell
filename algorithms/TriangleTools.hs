module TriangleTools (
	distant3,
	within,
	isRight,
	online,
	deleteOnline,
	draw
) where

import Graphics.UI.GLUT
import System.Environment

draw pl trs = do
	prgName <- getProgName
	rawArgs <- getArgs
	args <- initialize prgName rawArgs
	createWindow "GLTest"
	displayCallback $= do
		color $ (Color4 1 1 1 0 :: Color4 GLfloat)
		drawTriangles [((50, 50), (-50, -50), (0, 50))]
		color $ (Color4 1 0 0 0 :: Color4 GLfloat)
		drawPolyline [(50, 50), (-50, -50), (0, 50), (-50, 80)]
		swapBuffers
		flush
	mainLoop

main = do
	prgName <- getProgName
	rawArgs <- getArgs
	args <- initialize prgName rawArgs
	createWindow "GLTest"
	displayCallback $= displayScene
	mainLoop

displayScene = do
--	drawTriangles [(50, 50), (-50, -50), (0, 50)]
	drawPolyline [(50, 50), (-50, -50), (0, 50), (-50, 80)]
	swapBuffers
	flush

drawObject = preservingMatrix $
	renderPrimitive Lines $ mapM_ vertex [
		Vertex2 (-0.5) (-0.5),
		Vertex2 0.5 0.5 :: Vertex2 GLfloat ]

drawTriangles ps = preservingMatrix $
	renderPrimitive Triangles $ mapM_ vertex $ trianglesToVertex2 ps

trianglesToVertex2 :: [(Pos, Pos, Pos)] -> [Vertex2 GLfloat]
trianglesToVertex2 [] = []
trianglesToVertex2 ((a, b, c) : rest) =
	toVertex2 a : toVertex2 b : toVertex2 c : trianglesToVertex2 rest

toVertex2 :: (Double, Double) -> Vertex2 GLfloat
toVertex2 (x, y) = Vertex2
	(fromRational $ toRational x / 100) (fromRational $ toRational y / 100)

drawPolyline ps = preservingMatrix $
	renderPrimitive LineLoop $ mapM_ (vertex . toVertex2) ps

type Pos = (Double, Double)

isRight :: (Pos, Pos, Pos) -> Bool
isRight ((xa, ya), (xb, yb), (xc, yc))
	| 0 < xd * ye - xe * yd = True
	| 0 == xd * ye - xe * yd = error "bad triangle"
	| otherwise = False
	where
	(xd, yd) = (xa - xb, ya - yb)
	(xe, ye) = (xc - xb, yc - yb)

distance2 (x, y) = x ** 2 + y ** 2

maximum3 :: Ord a => [a] -> (a, a, a)
maximum3 xs = maximum3Gen $ last xs : xs ++ [head xs]

maximum3Gen :: Ord a => [a] -> (a, a, a)
maximum3Gen [x, y, z] = (x, y, z)
maximum3Gen (x : xs@(y : z : _))
	| y > y' = (x, y, z)
	| otherwise = p
	where
	p@(x', y', z') = maximum3Gen xs

maximumby3 :: Ord b => (a -> b) -> [a] -> (a, a, a)
maximumby3 b xs = maximumby3Gen b $ last xs : xs ++ [head xs]

maximumby3Gen :: Ord b => (a -> b) -> [a] -> (a, a, a)
maximumby3Gen _ [x, y, z] = (x, y, z)
maximumby3Gen b (x : xs@(y : z : _))
	| b y > b y' = (x, y, z)
	| otherwise = p
	where
	p@(_, y', _) = maximumby3Gen b xs

distant3 :: [Pos] -> (Pos, Pos, Pos)
distant3 = maximumby3 distance2

within :: (Pos, Pos, Pos) -> Pos -> Bool
within (a, b, c) d
	| online (d, a, b) || online (d, b, c) || online (d, c, a) = False
	| otherwise = isRight (a, b, c) &&
		isRight (d, a, b) && isRight (d, b, c) && isRight (d, c, a)

online :: (Pos, Pos, Pos) -> Bool
online ((xa, ya), (xb, yb), (xc, yc)) = 0 == xd * ye - xe * yd
	where
	(xd, yd) = (xa - xb, ya - yb)
	(xe, ye) = (xc - xb, yc - yb)

deleteOnline :: (Pos, Pos, Pos) -> (Pos, Pos)
deleteOnline (p1@(x1, _), p2@(x2, _), p3@(x3, _))
	| not $ online (p1, p2, p3) = error "not online"
	| x1 > x3 = deleteOnline (p3, p2, p1)
	| x2 < x1 = (p2, p3)
	| x1 <=x2 && x2 <= x3 = (p1, p3)
	| x3 < x2 = (p1, p2)
