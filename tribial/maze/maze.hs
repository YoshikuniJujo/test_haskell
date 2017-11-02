{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-unused-binds #-}

import Data.Bool
import Data.Char
import System.IO
import System.Random
import System.Environment

width = 40
height = 20

field n = makeStart (
	[],
	take height . map ([] ,) . groupsN width $ randoms (mkStdGen n) )
	where
	groupsN n [] = []
	groupsN n xs = take n xs : groupsN n (drop n xs)
	makeStart (as, (l, r) : bs) =
		(as, (l, replicate 4 False ++ drop 4 r) : bs)


showField (as, (l0, _ : r0) : bs) = unlines $
	map showLine (reverse as) ++ [showL l0 ++ "A" ++ showR r0] ++
	map showLine bs ++ [replicate (width - 2) ' ' ++ "GOAL"]
	where
	showLine (l, r) = showL l ++ showR r
	showL = map (bool ' ' '*') . reverse
	showR = map (bool ' ' '*')

move c f = case c of
	'h' -> left f
	'j' -> down f
	'k' -> up f
	'l' -> right f
	'H' -> leftf f
	'J' -> downf f
	'K' -> upf f
	'L' -> rightf f
	_ -> f

up f@((_, True : _) : _, _) = f
up f = upf f

down f@(_, _ : (_, True : _) : _) = f
down f = downf f

mapT2 f (x, y) = (f x, f y)

left f@(_, (True : _, _) : _) = f
left f = leftf f

right f@(_, (_, _ : True : _) : _) = f
right f = rightf f

upf f@([], _) = f
upf (a : as, bs) = (as, a : bs)

downf f@(_, [h]) = f
downf (as, h : bs) = (h : as, bs)

leftf f = ($ f) . mapT2 . map $ \lr -> case lr of
	([], rs) -> ([], rs)
	(l : ls, rs) -> (ls, l : rs)

rightf f = ($ f) . mapT2 . map $ \lr -> case lr of
	(ls, [h]) -> (ls, [h])
	(ls, h : rs) -> (h : ls, rs)

goal (_, [(_, [_])]) = True
goal _ = False

main = do
	n : _ <- getArgs
	let f0 = field $ read n
	print 100
	putStr $ showField f0
	noBuffering . loop (f0, 100) $ \(m0, p) -> do
		c <- getChar
		putStrLn ""
		case c of
			'q' -> return Nothing
			_ -> do	let	p' = if isUpper c then p - 10 else p
					m = move c m0
				print p'
				putStr $ showField m
				case (goal m, p' <= 0) of
					(_, True) -> do
						putStrLn "YOU LOSE!"
						return Nothing
					(True, False) ->do
						putStrLn "YOU WIN!"
						return Nothing
					(False, False) -> return $ Just (m, p')

noBuffering act = do
	bi <- hGetBuffering stdin
	bo <- hGetBuffering stdout
	hSetBuffering stdin NoBuffering
	hSetBuffering stdout NoBuffering
	act
	hSetBuffering stdin bi
	hSetBuffering stdout bo

loop s0 act = do
	ms1 <- act s0
	case ms1 of
		Just s1 -> loop s1 act
		Nothing -> return ()
