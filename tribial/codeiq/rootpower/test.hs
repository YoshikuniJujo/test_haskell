{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow

main :: IO ()
main = interact $ show . (\(N ((n, Root []) : _)) -> n) .
	(\e -> expMod base e (10 ^ (7 :: Int))) . read

base :: N
base = N $ ((1 ,) . Root) <$> [[], [2], [3], [5]]

expMod :: N -> Integer -> Integer -> N
expMod _ 0 _ = 1
expMod b e m
	| even e = expMod (b ^ (2 :: Int) `modN` m) (e `div` 2) m
	| otherwise = b * expMod b (e - 1) m `modN` m

-- N

data N = N [(Integer, Root)] deriving Show

infixl 7 `modN`

modN :: Integral n => N -> n -> N
modN (N jxs) n = N $ first (`mod` fromIntegral n) <$> jxs

instance Num N where
	N jxs + N kys = N $ jxs `add` kys
	N jxs * N kys = jxs `mul` kys
	negate (N jxs) = N $ first negate <$> jxs
	abs = (*) <$> signum <*> id
	signum n = case toDouble n of 0 -> 0; r | r < 0 -> - 1; _ -> 1
	fromInteger = N . (: []) . (, Root [])

instance IsDouble N where
	toDouble (N jxs) =
		sum $ uncurry (*) . (fromIntegral *** toDouble) <$> jxs

add :: [(Integer, Root)] -> [(Integer, Root)] -> [(Integer, Root)]
jxa@(jx@(j, x) : jxs) `add` kya@(ky@(k, y) : kys)
	| x < y = jx : (jxs `add` kya)
	| x > y = ky : (jxa `add` kys)
	| otherwise = (j + k, x) : (jxs `add` kys)
jxa `add` [] = jxa
_ `add` kya = kya

mul :: [(Integer, Root)] -> [(Integer, Root)] -> N
(jx@(j, x) : jxs) `mul` (ky@(k, y) : kys) =
	N [(j * k *) `first` (x `mulR` y)] +
	[jx] `mul` kys + jxs `mul` [ky] + jxs `mul` kys
mul _ _ = N []

-- Root

data Root = Root [Integer] deriving (Show, Eq, Ord)

mulR :: Root -> Root -> (Integer, Root)
Root r1 `mulR` Root r2 = Root `second` (r1 `ml` r2)
	where
	ml ma@(m : ms) na@(n : ns)
		| m < n = (m :) `second` ml ms na
		| m > n = (n :) `second` ml ma ns
		| otherwise = (m *) `first` ml ms ns
	ml ma [] = (1, ma)
	ml [] na = (1, na)

class IsDouble d where toDouble :: d -> Double

instance IsDouble Root where
	toDouble (Root ns) = fromIntegral (product ns) ** (1 / 2)
