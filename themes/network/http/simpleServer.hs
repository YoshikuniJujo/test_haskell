import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.List
import Data.Char
import Network
import System.IO

main :: IO ()
main = do
	cnt <- readFile "test.html"
	print cnt
	soc <- listenOn $ PortNumber 4492
	forever $ do
		(h, _, _) <- accept soc
		hGetEncoding h >>= print
		hSetEncoding h =<< mkTextEncoding "utf-8"
		void . forkIO $ do
			(r, ms) <- getRequest h
			print r
			(flip . maybe $ return ()) ms $ \s -> replicateM_ s $
				hGetChar h >>= putChar
			hPutStrLn h "HTTP/1.1 200 OK"
			hPutStrLn h "Content-Type: text/html; charset=UTF-8"
			hPutStrLn h $ "Content-Length: " ++ show (length cnt)
			hPutStrLn h "Content-Language: ja"
			hPutStrLn h ""
			hPutStrLn h cnt

getRequest :: Handle -> IO ([String], Maybe Int)
getRequest h = do
	ls <- toEmptyLine h
	return (ls, (read . dropWhile (not . isSpace) <$>) . listToMaybe
		$ filter ("Content-Length: " `isPrefixOf`) ls)

toEmptyLine :: Handle -> IO [String]
toEmptyLine h = do
	l <- hGetLine h
	case l of
		"\r" -> return []
		_ -> (l :) <$> toEmptyLine h
