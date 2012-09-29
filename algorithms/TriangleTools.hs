module TriangleTools (
	Pos,
	far,
--	maximumIndex,
--	distance2,
	index3,
	deleteIndex,

	distant3,
	within,
	isRight,
	online,
	deleteOnline,
	draw,

	pa, pb, pc, pd, pe
) where

import Graphics.UI.GLUT
import System.Environment

far :: [Pos] -> Int
far = maximumIndex . map distance2

deleteOnline xs = init $ tail $ dol $ last xs : xs ++ [head xs]

dol :: [Pos] -> [Pos]
dol [a, b] = [a, b]
dol (a : ps@(b : ps'@(c : _)))
	| online (a, b, c) = a : dol ps'
	| otherwise = a : dol ps

maximumIndex :: Ord a => [a] -> Int
maximumIndex = fst . maximumIndexGen

index3 :: [a] -> Int -> (a, a, a)
index3 xs i = (xs' !! i, xs' !! (i + 1), xs' !! (i + 2))
	where
	xs' = last xs : xs ++ [head xs]

deleteIndex :: [a] -> Int -> [a]
deleteIndex xs i = take i xs ++ drop (i + 1) xs

maximumIndexGen :: Ord a => [a] -> (Int, a)
maximumIndexGen [x] = (0, x)
maximumIndexGen (x : xs)
	| x >= x' = (0, x)
	| otherwise = (i + 1, x')
	where
	(i, x') = maximumIndexGen xs

draw pl trs = do
	prgName <- getProgName
	rawArgs <- getArgs
	args <- initialize prgName rawArgs
	createWindow "GLTest"
	displayCallback $= do
		color $ (Color4 1 1 1 0 :: Color4 GLfloat)
		drawTriangles trs -- [((50, 50), (-50, -50), (0, 50))]
		color $ (Color4 1 0 0 0 :: Color4 GLfloat)
		drawPolyline pl -- [(50, 50), (-50, -50), (0, 50), (-50, 80)]
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
	(fromRational $ toRational x / 20) (fromRational $ toRational y / 20)

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

maximumby32 b xs = maximumby3Gen2 b $ last xs : xs ++ [head xs]

maximumby3Gen2 :: Ord b => (a -> b) -> [a] -> ((a, a, a), [a])
maximumby3Gen2 _ [x, y, z] = ((x, y, z), [x, z])
maximumby3Gen2 b (x : ys@(y : (zs@(z : _))))
	| b y > b y' = ((x, y, z), x : zs)
	| otherwise = (t, x : ps)
	where
	(t@(_, y', _), ps) = maximumby3Gen2 b ys

distant3 :: [Pos] -> ((Pos, Pos, Pos), [Pos])
distant3 ps = let (t, ps') = maximumby32 distance2 ps in (t, init $ tail ps')

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

deleteOnlineGen :: (Pos, Pos, Pos) -> (Pos, Pos)
deleteOnlineGen (p1@(x1, _), p2@(x2, _), p3@(x3, _))
	| not $ online (p1, p2, p3) = error "not online"
	| x1 > x3 = deleteOnlineGen (p3, p2, p1)
	| x2 < x1 = (p2, p3)
	| x1 <=x2 && x2 <= x3 = (p1, p3)
	| x3 < x2 = (p1, p2)

pa, pb, pc, pd, pe :: Pos
pa = (0, 4)
pb = (4, 4)
pc = (4, 0)
pd = (3, 3)
pe = (2, 2)

{-
deleteOnline :: [Pos] -> [Pos]
deleteOnline xs = deleteOnline' $ last xs : xs ++ [head xs]

deleteOnline' :: [Pos] -> [Pos]
deleteOnline' ps@[_, _] = ps
deleteOnline' (a : ps@(b : c : ps'))
	| online (a, b, c) = deleteOnline' $ a' : b' : ps'
	| otherwise = a : deleteOnline' ps
	where
	(a', b') = deleteOnlineGen (a, b, c)
-}
