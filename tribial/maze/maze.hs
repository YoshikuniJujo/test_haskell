{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-unused-binds #-}

import Data.Bool
import Data.Char
import System.IO
import System.Random
import System.Environment

point0 = 100
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

goal (_, [(_, [_])]) = True
goal _ = False

main = do
	n : _ <- getArgs
	let f0 = field $ read n
	print point0
	putStr $ showField f0
	noBuffering . loop (f0, point0) $ \(f, p) -> do
		c <- getChar
		putStrLn ""
		case c of
			'q' -> return Nothing
			_ -> do	let	p' = if isUpper c then p - 10 else p
					f' = move c f
				print p'
				putStr $ showField f'
				case (goal f', p' <= 0) of
					(_, True) -> do
						putStrLn "YOU LOSE!"
						return Nothing
					(True, False) ->do
						putStrLn "YOU WIN!"
						return Nothing
					(False, False) -> return $ Just (f', p')

noBuffering act = do
	bi <- hGetBuffering stdin
	bo <- hGetBuffering stdout
	hSetBuffering stdin NoBuffering
	hSetBuffering stdout NoBuffering
	act
	hSetBuffering stdin bi
	hSetBuffering stdout bo

loop s act = do
	ms' <- act s
	case ms' of
		Just s' -> loop s' act
		Nothing -> return ()
