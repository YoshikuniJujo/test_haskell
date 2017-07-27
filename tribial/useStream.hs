import Prelude hiding (readFile)

import System.IO (IOMode(..), openFile, hIsEOF, hGetLine)
import Stream

readFile :: FilePath -> Stream IO String ()
readFile fp = do
	hdl <- liftIO $ openFile fp ReadMode
	rh hdl
	where
	rh h = do
		e <- liftIO $ hIsEOF h
		if e then return () else do
			liftIO (hGetLine h) >>= yield
			rh h

printNLines :: Stream IO String a -> Int -> IO ()
printNLines _ n | n < 1 = return ()
printNLines str n = do
	r <- runStream str
	case r of
		Left _ -> return ()
		Right (l, str') -> do
			putStrLn l
			printNLines str' (n - 1)
