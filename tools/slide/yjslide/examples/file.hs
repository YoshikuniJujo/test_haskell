import System.IO

printFile :: FilePath -> IO ()
printFile fp = withFile fp ReadMode $ \h -> do
	eof <- hIsEOF h
	if eof then return () else hGetLine h >>= putStrLn
