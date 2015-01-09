import System.IO.Unsafe
import Data.IORef
import Control.Concurrent
import System.Random

single :: IORef Int
single = unsafePerformIO $ newIORef 0

getSingle :: IO Int
getSingle = readIORef single

setSingle :: Int -> IO ()
setSingle = writeIORef single

modifySingle :: (Int -> Int) -> IO ()
modifySingle m = setSingle . m =<< getSingle

randomDelay :: IO ()
randomDelay = do
	d <- randomRIO (0, 100000)
--	print d
	threadDelay d

trial :: IO ()
trial = do
	setSingle 8
	t <- getSingle
	forkIO $ randomDelay >> modifySingle (+ 10)
	forkIO $ randomDelay >> modifySingle (* 10)
	randomDelay
	getSingle >>= print

timesDo_ :: Int -> IO a -> IO ()
timesDo_ 0 _ = return ()
timesDo_ n io = io >> timesDo_ (n - 1) io

main :: IO ()
main = 10 `timesDo_` trial
