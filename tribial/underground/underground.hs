{-# OPTIONS_GHC -fno-warn-tabs #-}

import System.IO

width = 80

main = do
	hSetBuffering stdin NoBuffering
	hSetBuffering stdout NoBuffering
	loop 0 $ \s0 -> do
		c <- getChar
		putStrLn ""
		case c of
			'q' -> return Nothing
			'h' -> do
				putStr (replicate (s0 - 1) ' ' ++ "*" ++ replicate (width - s0 + 1) ' ')
				return (Just (s0 - 1))
			'l' -> do
				putStr (replicate (s0 + 1) ' ' ++ "*" ++ replicate (width - s0 - 1) ' ')
				return (Just (s0 + 1))
			_ -> do	putStr (replicate s0 ' ' ++ "*" ++ replicate (width - s0) ' ')
				return (Just s0)

loop :: s -> (s -> IO (Maybe s)) -> IO ()
loop s0 act = do
	ms1 <- act s0
	case ms1 of
		Just s1 -> loop s1 act
		Nothing -> return ()
