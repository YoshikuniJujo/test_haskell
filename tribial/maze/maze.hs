{-# OPTIONS_GHC -fno-warn-tabs -fwarn-unused-binds #-}

import Data.Bool
import Data.Char
import System.IO
import System.Environment

import Field

point0 = 100

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

display p f = do
	putStrLn ""
	print p
	putStr $ showField f

main = do
	n : _ <- getArgs
	let f0 = field $ read n
	display point0 f0
	noBuffering . loop (point0, f0) $ \(p, f) -> do
		c <- getChar
		case c of
			'q' -> return Nothing
			_ -> next c p f

next c p f = do
	display p' f'
	case (goal f', p' <= 0) of
		(_, True) -> do
			putStrLn "YOU LOSE!"
			return Nothing
		(True, False) -> do
			putStrLn "YOU WIN!"
			return Nothing
		(False, False) -> return $ Just (p', f')
	where
	p' = bool p (p - 10) (isUpper c)
	f' = move c f

noBuffering act = do
	bi <- hGetBuffering stdin
	hSetBuffering stdin NoBuffering
	act
	hSetBuffering stdin bi

loop s act = do
	ms' <- act s
	case ms' of
		Just s' -> loop s' act
		Nothing -> return ()
