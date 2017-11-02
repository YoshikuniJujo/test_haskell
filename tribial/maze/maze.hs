{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Bool
import Data.Char
import System.IO
import System.Random
import System.Environment

width = 40
height = 20

groupsN n [] = []
groupsN n xs = take n xs : groupsN n (drop n xs)

-- field :: ([([Bool], [Bool])], [([Bool], [Bool])])
field n = makeStart (
	[],
	take height . map ([] ,) . groupsN width $ randoms (mkStdGen n) )

makeStart :: ([([Bool], [Bool])], [([Bool], [Bool])]) ->
	([([Bool], [Bool])], [([Bool], [Bool])])
makeStart (as, (l, r) : bs) = (as, (l, replicate 4 False ++ drop 4 r) : bs)

showField (as, (l0, _ : r0) : bs) = unlines $
	map (map (bool ' ' '*') . (\(l, r) -> reverse l ++ r)) (reverse as) ++
	[map (bool ' ' '*') (reverse l0) ++ "A" ++ map (bool ' ' '*') r0] ++
	map (map (bool ' ' '*') . (\(l, r) -> reverse l ++ r)) bs ++
	[replicate (width - 2) ' ' ++ "GOAL"]

up f@([], _) = f
up f@((_, True : _) : _, _) = f
up (a : as, bs) = (as, a : bs)

down f@(_, [h]) = f
down f@(_, _ : (_, True : _) : _) = f
down (as, h : bs) = (h : as, bs)

mapT2 f (x, y) = (f x, f y)

left f@(_, (True : _, _) : _) = f
left f = ($ f) . mapT2 . map $ \lr -> case lr of
	([], rs) -> ([], rs)
	(l : ls, rs) -> (ls, l : rs)

right f@(_, (_, _ : True : _) : _) = f
right f = ($ f) . mapT2 . map $ \lr -> case lr of
	(ls, [h]) -> (ls, [h])
	(ls, h : rs) -> (h : ls, rs)

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

main' = noBuffering . loop (0, 0) $ \(x0, y0) -> do
	c <- getChar
	putStrLn ""
	case c of
		'q' -> return Nothing
		_ -> do let (x, y) = move' c x0 y0
			putStr $ showMaze x y
			return $ Just (x, y)

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

showMaze x y = unlines $
	replicate y (replicate width ' ') ++
	[replicate x ' ' ++ "A" ++ replicate (width - x - 1) ' '] ++
	replicate (height - y - 1) (replicate width ' ')

bound mn mx x
	| x < mn = mn
	| x > mx = mx
	| otherwise = x

move' c x y = (bound 0 (width - 1) x', bound 0 (height - 1) y')
	where
	(x', y') = case c of
		'h' -> (x - 1, y)
		'j' -> (x, y + 1)
		'k' -> (x, y - 1)
		'l' -> (x + 1, y)
		_ -> (x, y)
