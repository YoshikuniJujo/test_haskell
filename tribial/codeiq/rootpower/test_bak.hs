import Control.Applicative

data Root = Int Int | Root Int Int deriving Show

data N = N [Root] deriving Show

instance Num N where
	fromInteger = N . (: []) . Int . fromIntegral
	abs = (*) <$> signum <*> id
	signum n = case signum $ toDouble n of
		-1 -> -1
		0 -> 0
		_ -> 1
	negate (N rs) = N $ neg rs
	N rs1 + N rs2 = N $ rs1 `add` rs2
	N rs1 * N rs2 = N $ rs1 `mul` rs2

add :: [Root] -> [Root] -> [Root]
add (Int i1 : rs1) (Int i2 : rs2) = Int (i1 + i2) : add rs1 rs2
add (Int i1 : rs1) ra2 = Int i1 : add rs1 ra2
add ra1 (Int i2 : rs2) = Int i2 : add ra1 rs2
add ra1@(Root k1 n1 : rs1) ra2@(Root k2 n2 : rs2)
	| n1 < n2 = Root k1 n1 : add rs1 ra2
	| n1 > n2 = Root k2 n2 : add ra1 rs2
	| otherwise = Root (k1 + k2) n1 : add rs1 rs2
add ra1 [] = ra1
add [] ra2 = ra2

mul :: [Root] -> [Root] -> [Root]
mul ra1 [] = ra1
mul [] ra2 = ra2

neg :: [Root] -> [Root]
neg (Int i : rs) = Int (- i) : neg rs
neg (Root k n : rs) = Root (- k) n : neg rs
neg _ = []

toDouble :: N -> Double
toDouble (N rs0) = tod rs0
	where
	tod (Int i : rs) = fromIntegral i + tod rs
	tod (Root k n : rs) =
		fromIntegral k * fromIntegral n ** (1 / 2) + tod rs
	tod _ = 0
