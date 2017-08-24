import Control.Concurrent
import Control.Concurrent.Chan
import System.IO
import Network

main :: IO ()
main = do
	c <- newChan
	forkIO . loopS (0, 0) $ \(k, t) -> do
		ans <- readChan c
		let	(k', t') = case ans of
				"kinoko" -> (k + 1, t)
				"takenoko" -> (k, t + 1)
				_ -> (k, t)
		putStrLn $ "kinoko:   " ++ replicate k' '*' ++ " (" ++ show k' ++ ")"
		putStrLn $ "takinoko: " ++ replicate t' '*' ++ " (" ++ show t' ++ ")"
		return (k', t')
	s <- listenOn $ PortNumber 4492
	loop $ do
		(h, hn, pn) <- accept s
		print hn
		print pn
		forkIO $ do
			hGetLine h >>= putStrLn . ("name  : " ++)
			kt <- hGetLine h
			putStrLn $ "answer: " ++ kt
			writeChan c kt

loop :: IO a -> IO ()
loop act = act >> loop act

loopS :: a -> (a -> IO a) -> IO ()
loopS s act = do
	s' <- act s
	loopS s' act
