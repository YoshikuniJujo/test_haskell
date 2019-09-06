{-# LANGUAGE CApiFFI, CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Lib

main :: IO ()
main = do
	let	dat = 12345
	() <$ fprintf_d stderr "Sending data = %d\n" dat
	() <$ printf_ "Not explicitly sending a credentials structure\n"
	srvfd <- socket afUnix sockStream protocol0
	withSockaddrUn $ \sau -> do
		pokeSunFamily sau saFamilyTAfUnix
		pokeSunPathString sau sampleUnixDomainPath
		connect srvfd sau
	withSimpleMsghdr ["Hello, ", "world!"]
		$ \msgh -> sendmsg srvfd msgh msgFlags0
	close srvfd
