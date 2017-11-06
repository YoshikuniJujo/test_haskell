{-# OPTIONS_GHC -fno-warn-tabs -fwarn-unused-binds #-}

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
