import System.IO
import Network

main :: IO ()
main = do
	h <- connectTo "skami.iocikun.jp" $ PortNumber 4492
	hPutStrLn h "Yoshio"
	hPutStrLn h "takenoko"
	hGetLine h >>= putStrLn
	hClose h
