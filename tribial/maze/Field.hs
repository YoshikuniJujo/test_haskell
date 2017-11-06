{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Field (
	field, showField, goal,
	up, down, left, right,
	upf, downf, leftf, rightf ) where

import Data.Bool
import System.Random

width = 40
height = 20

field = toField . space4 . take height . divide width . randoms . mkStdGen
	where
	toField ls = ([], map ([] ,) ls)
	space4 (l : ls) = (replicate 4 False ++ drop 4 l) : ls
	divide n xs = take n xs : divide n (drop n xs)

showField (t, (l, _ : r) : b) = unlines $
	map showLine (reverse t) ++
	[showL l ++ "A" ++ showR r] ++
	map showLine b ++
	[replicate (width - 2) ' ' ++ "GOAL"]
	where
	showLine (l, r) = showL l ++ showR r
	showL = map (bool ' ' '*') . reverse
	showR = map (bool ' ' '*')

goal (_, [(_, [_])]) = True
goal _ = False

up f@((_, True : _) : _, _) = f
up f = upf f

down f@(_, _ : (_, True : _) : _) = f
down f = downf f

left f@(_, (True : _, _) : _) = f
left f = leftf f

right f@(_, (_, _ : True : _) : _) = f
right f = rightf f

upf f@([], _) = f
upf (a : as, bs) = (as, a : bs)

downf f@(_, [h]) = f
downf (as, h : bs) = (h : as, bs)

leftf = mapT2 . map $ \lhr -> case lhr of
	([], _) -> lhr
	(l : ls, hrs) -> (ls, l : hrs)

rightf = mapT2 . map $ \lhr -> case lhr of
	(_, [_]) -> lhr
	(ls, h : rs) -> (h : ls, rs)

mapT2 f (x, y) = (f x, f y)
