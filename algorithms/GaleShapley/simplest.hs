import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List

data Man = A | B | C | D deriving (Show, Eq)
data Woman = X | Y | Z | W deriving (Show, Eq)

type ManT = [(Man, [Woman])]
type WomanT = [(Woman, [Man])]
type WomanR = [((Woman, Man), Int)]

manT :: ManT
manT = [
	(A, [Y, X, Z, W]),
	(B, [X, W, Y, Z]),
	(C, [Y, X, Z, W]),
	(D, [W, Z, Y, X]) ]

womanT :: WomanT
womanT = [
	(X, [C, D, B, A]),
	(Y, [D, B, A, C]),
	(Z, [A, C, B, D]),
	(W, [C, B, A, D]) ]

womanR :: WomanR
womanR = rtable womanT

rtable :: WomanT -> WomanR
rtable = concatMap rtable1

rtable1 :: (Woman, [Man]) -> WomanR
rtable1 (w, ms) = zipWith (\m p -> ((w, m), p)) ms [4, 3, 2, 1]

womanPoint :: WomanR -> Woman -> Man -> Int
womanPoint wr w m = fromJust $ lookup (w, m) wr

type Pair = (Woman, (Man, Int))
type State = (ManT, [Pair])

isSingle :: Man -> [Pair] -> Bool
isSingle m ps = m `notElem` map (fst . snd) ps

single :: ManT -> [Pair] -> Maybe ((Man, [Woman]), ManT)
single [] _ = Nothing
single (mp@(m, _) : mps) ps
	| isSingle m ps = Just (mp, mps)
	| True = second (mp :) <$> single mps ps

step :: WomanR -> ManT -> [Pair] -> Maybe State
step wr mt ps = case single mt ps of
	Just ((m, w : ws), mt') -> case partition ((== w) . fst) ps of
		([(_, (m', p'))], ps')
			| p > p' -> Just (mt'', (w, (m, p)) : ps')
			| True -> Just (mt'', ps)
		_ -> Just (mt'', (w, (m, p)) : ps)
		where
		mt'' = ((m, ws) : mt')
		p = womanPoint wr w m
	_ -> Nothing

run :: (s -> Maybe s) -> s -> s
run n s = maybe s (run n) $ n s

pairs :: ManT -> WomanT -> [Pair]
pairs mt wt = snd $ run (uncurry $ step (rtable wt)) (mt, [])
