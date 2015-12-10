import Data.Char

---------------------------------------------------------------------------
-- Input and Output

main :: IO ()
main = interact $ (++ "\n") . map toLower . show . uncurry relativity
	. (\(t1 : t2 : _) -> (t1, t2)) . map readTriangle . lines

readTriangle :: String -> Triangle
readTriangle s = let [a, b, c] = map readPoint $ words s in Triangle a b c

readPoint :: String -> Point
readPoint s = let (x, ',' : y) = span (/= ',') s in Point (read x) (read y)

---------------------------------------------------------------------------
-- Vectors and Points

data LR = L | R deriving (Eq, Show)

data Vector = Vector Int Int deriving Show
data Point = Point Int Int deriving Show

sub :: Point -> Point -> Vector
Point ax ay `sub` Point bx by = Vector (ax - bx) (ay - by)

mul :: Vector -> Vector -> Int
Vector ax ay `mul` Vector bx by = ax * by - ay * bx

pointVectorLR :: Point -> (Point, Vector) -> Maybe LR
pointVectorLR (Point px py) (Point sx sy, Vector vx vy)
	| vx * (py - sy) > vy * (px - sx) = Just L
	| vx * (py - sy) < vy * (px - sx) = Just R
	| otherwise = Nothing

---------------------------------------------------------------------------
-- Lines

data Line = Line Point Point deriving Show

toVector :: Line -> (Point, Vector)
toVector (Line p1 p2) = (p1, p2 `sub` p1)

pointLineLR :: Point -> Line -> Maybe LR
pointLineLR p = pointVectorLR p . toVector

cross :: Line -> Line -> Bool
cross ab@(Line a b) cd@(Line c d) =
	pointLineLR c ab /= pointLineLR d ab &&
	pointLineLR a cd /= pointLineLR b cd

---------------------------------------------------------------------------
-- Triangles

data Triangle = Triangle Point Point Point deriving Show

triangleLR :: Triangle -> Maybe LR
triangleLR (Triangle a b c)
	| (b `sub` a) `mul` (c `sub` a) > 0 = Just L
	| (b `sub` a) `mul` (c `sub` a) < 0 = Just R
	| otherwise = Nothing

leftize :: Triangle -> Triangle
leftize t@(Triangle a b c)
	| Just R <- triangleLR t = Triangle a c b
	| otherwise = t

toLines :: Triangle -> [Line]
toLines (Triangle a b c) = [Line a b, Line b c, Line c a]

---------------------------------------------------------------------------
-- Relativities

data Relativity = Cross | Inside | Outside deriving (Show, Eq)

pointTriangleRelativity :: Point -> Triangle -> Relativity
pointTriangleRelativity p t
	| Nothing `elem` rs = Cross
	| all (== Just L) rs = Inside
	| otherwise = Outside
	where rs = map (pointLineLR p) . toLines $ leftize t

relativity :: Triangle -> Triangle -> Relativity
relativity t1 t2 = case (rel t1 t2, rel t2 t1) of
	(Inside, _) -> Inside
	(_, Inside) -> Inside
	(Outside, Outside) ->
		if or [ l1 `cross` l2 | l1 <- toLines t1, l2 <- toLines t2 ]
			then Cross else Outside
	_ -> Cross
	where
	rel (Triangle a b c) t
		| all (== Inside) rs = Inside
		| all (== Outside) rs = Outside
		| otherwise = Cross
		where rs = map (`pointTriangleRelativity` t) [a, b, c]
