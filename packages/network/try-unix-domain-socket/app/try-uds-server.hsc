{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#define _GNU_SOURCE

#include <sys/socket.h>

module Main where

import Control.Concurrent (forkFinally)
import Control.Monad (unless, forever, void)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

import qualified Control.Exception as Ex
import qualified Data.ByteString as BS

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

main :: IO ()
main = withSocketsDo $ do
	print isUnixDomainSocketAvailable
	sock <- socket AF_UNIX Stream 0
	Ex.bracket (open sock) close loop
	where
	open sock = do
		setSocketOption sock ReuseAddr 1
		setCloseOnExecIfNeeded $ fdSocket sock
		bind sock $ SockAddrUnix "/tmp/foo.sock"
		listen sock 10
		return sock
	loop sock = forever $ do
		(conn, peer) <- accept sock
		getPeerUCredRaw sock >>= print
		getPeerUCredRaw conn >>= print
		getPeerUCredPtr sock >>= \uc -> do
			print uc
--			peekPid uc >>= print
--			peekUid uc >>= print
--			peekGid uc >>= print
		putStrLn $ "Connection from " ++ show peer
		void $ forkFinally (talk conn) (\_ -> close conn)
	talk conn = do
		msg <- recv conn 1024
		unless (BS.null msg) $ do
			sendAll conn msg
			talk conn

getPeerUCredRaw :: Socket -> IO Int
getPeerUCredRaw = (`getSocketOption` CustomSockOpt (#{const SOL_SOCKET}, #{const SO_PEERCRED}))

getPeerUCredPtr :: Socket -> IO (Ptr UCred)
getPeerUCredPtr = (intToPtr <$>) . (`getSocketOption` CustomSockOpt (#{const SOL_SOCKET}, #{const SO_PEERCRED}))

intToPtr :: Int -> Ptr a
intToPtr = intPtrToPtr . IntPtr

data UCred

peekPid, peekUid, peekGid :: Ptr UCred -> IO CInt
peekPid = #{peek struct ucred, pid}
peekUid = #{peek struct ucred, uid}
peekGid = #{peek struct ucred, gid}
