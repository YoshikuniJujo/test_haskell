{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Foreign.C.String
import Foreign.Storable
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
				clifd <- accept lsnfd sau
				print clifd
				setsockopt clifd solSocket soPasscred soPasscredTrue
				withMsghdrUcredServer [126]
						(\msgh -> recvmsg
							clifd msgh msgFlags0)
					$ \msgh ucredp -> do
						iov0 <- c_peekMsgIov msgh
						print iov0
						iovn <- c_peekMsgIovlen msgh
						print iovn
						iov0base <- c_peekIovBase iov0
						c_peekIovLen iov0 >>= print
						print =<< peekCString iov0base
						print =<< peek ucredp
		_ -> putStrLn "bad"
	close lsnfd
