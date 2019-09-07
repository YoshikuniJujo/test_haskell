{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Directory

import Lib

main :: IO ()
main = do
	lsnfd <- socket afUnix sockStream protocol0
	removeFile sampleUnixDomainPath
	withSockaddrUn $ \sau -> do
		pokeSunFamily sau saFamilyTAfUnix
		pokeSunPathString sau sampleUnixDomainPath
	close lsnfd
