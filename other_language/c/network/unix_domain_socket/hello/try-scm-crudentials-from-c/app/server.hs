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
	poll [Pollfd lsnfd pollin Nothing] Nothing >>= print
	close lsnfd
