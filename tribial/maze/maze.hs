{-# OPTIONS_GHC -fno-warn-tabs #-}

import System.IO

width = 70
height = 20

main = noBuffering . loop (0, 0) $ \(x0, y0) -> do
	c <- getChar
	putStrLn ""
	case c of
		'q' -> return Nothing
		_ -> do let (x, y) = move c x0 y0
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

move c x y = (bound 0 (width - 1) x', bound 0 (height - 1) y')
	where
	(x', y') = case c of
		'h' -> (x - 1, y)
		'j' -> (x, y + 1)
		'k' -> (x, y - 1)
		'l' -> (x + 1, y)
		_ -> (x, y)
