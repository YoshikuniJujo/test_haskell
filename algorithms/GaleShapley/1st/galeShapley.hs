import Control.Arrow
import Data.Maybe

data Man = A | B | C | D deriving (Show, Eq, Ord)
data Woman = X | Y | Z | W deriving (Show, Eq, Ord)
data PointM = PM Int deriving (Show, Eq, Ord)
data PointW = PW Int deriving (Show, Eq, Ord)

type ManP = [(Man, [(Woman, PointM)])]
type WomanP = [(Woman, [(Man, PointW)])]

type Pairs = [(Woman, (Man, PointW))]
type State = (ManP, Pairs)

manP :: ManP
manP = [
	(A, [(Y, PM 10), (X, PM 8), (Z, PM 5), (W, PM 2)]),
	(B, [(X, PM 21), (W, PM 15), (Y, PM 9), (Z, PM 1)]),
	(C, [(Y, PM 13), (X, PM 9), (Z, PM 4), (W, PM 3)]),
	(D, [(W, PM 18), (Z, PM 11), (Y, PM 8), (X, PM 4)]) ]

womanP :: WomanP
womanP = [
	(X, [(C, PW 28), (D, PW 21), (B, PW 10), (A, PW 4)]),
	(Y, [(D, PW 5), (B, PW 4), (A, PW 2), (C, PW 1)]),
	(Z, [(A, PW 10), (C, PW 9), (B, PW 5), (D, PW 3)]),
	(W, [(C, PW 19), (B, PW 17), (A, PW 10), (D, PW 2)]) ]

womanPoint :: WomanP -> Woman -> Man -> PointW
womanPoint wp w m = fromJust $ lookup m =<< lookup w wp

selectWoman :: [(Woman, PointM)] -> ((Woman, PointM), [(Woman, PointM)])
selectWoman (wp : wps) = sw wp wps
	where
	sw (w, p) [] = ((w, p), [])
	sw wp0@(_, p0) (wp@(_, p) : wps)
		| p > p0 = (second (wp0 :)) $ sw wp wps
		| True = (second (wp :)) $ sw wp0 wps

singleMan ::  ManP -> Pairs -> ((Man, [(Woman, PointM)]), ManP)
singleMan (mp@(m, _) : mps) ps
	| m `elem` map (fst . snd) ps = (mp :) `second` singleMan mps ps
	| True = (mp, mps)

-- propose :: WomanP -> Man -> Woman -> Pairs -> Pairs
-- propose wp m w ps
--	| w `notElem` map fst ps = (w, (m, womanPoint wp w m)) : ps

-- step :: WomanP -> State -> State
-- step wp (mp, pr)
--	| 
