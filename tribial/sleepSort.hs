import Control.Concurrent

sleepSort :: [Int] -> IO ()
sleepSort [] = return ()
sleepSort (n : ns) = do
	forkIO $ do
		threadDelay (n * 1000)
		print n
	sleepSort ns
