{-# LANGUAGE CApiFFI, CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import System.Posix.Process
import System.Posix.User

import Lib

main :: IO ()
main = do
	args <- getArgs
	let	argc = length args
	gid <- if argc > 0 then readIO (args !! 0) else getRealGroupID
	uid <- if argc > 1 then readIO (args !! 1) else getRealUserID
	pid <- if argc > 2 then readIO (args !! 2) else getProcessID
	let	ucred = Ucred pid uid gid
	srvfd <- socket afUnix sockStream protocol0
	withSockaddrUn $ \sau -> do
		pokeSunFamily sau saFamilyTAfUnix
		pokeSunPathString sau sampleUnixDomainPath
		connect srvfd sau
	withMsghdrUcred ["Hello, ", "world!"] ucred
		$ \msgh -> sendmsg srvfd msgh msgFlags0
	close srvfd
