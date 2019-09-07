{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Directory

import Lib
import Server

main :: IO ()
main = do
	lsnfd <- socket afUnix sockStream protocol0
	removeFile sampleUnixDomainPath
	withSockaddrUn $ \sau -> do
		pokeSunFamily sau saFamilyTAfUnix
		pokeSunPathString sau sampleUnixDomainPath
		bind lsnfd sau
	listen lsnfd 5
	(_pon, pos) <- poll [Pollfd lsnfd pollin Nothing] Nothing
	print pos
	case pos of
		[Pollfd _ _ (Just po)]
			| po == pollin -> withSockaddrUn $ \sau -> do
				accept lsnfd sau >>= print
		_ -> putStrLn "bad"
	close lsnfd
