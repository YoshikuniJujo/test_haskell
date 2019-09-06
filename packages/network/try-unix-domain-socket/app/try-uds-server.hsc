{-# LANGUAGE CPP, ForeignFunctionInterface, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#define _GNU_SOURCE

#include <sys/socket.h>

module Main where

import Control.Concurrent (forkFinally)
import Control.Monad (unless, forever, void)
import Network.Socket
import Network.Socket.Internal
import Network.Socket.ByteString (recv, sendAll)

import qualified Control.Exception as Ex
import qualified Data.ByteString as BS

import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable
import Foreign.C.Types

foreign import ccall unsafe "getsockopt"
	c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt

main :: IO ()
main = withSocketsDo $ do
	print isUnixDomainSocketAvailable
	sock <- socket AF_UNIX Stream 0
	Ex.bracket (open sock) close loop
	where
	open sock = do
		setSocketOption sock ReuseAddr 1
		withFdSocket sock $ setCloseOnExecIfNeeded
		bind sock $ SockAddrUnix "/tmp/foo.sock"
		listen sock 10
		return sock
	loop sock = forever $ do
		(conn, peer) <- accept sock
		getSocketOption conn (CustomSockOpt (#{const SOL_SOCKET}, #{const SO_PASSCRED})) >>= print
		setSocketOption conn (CustomSockOpt (#{const SOL_SOCKET}, #{const SO_PASSCRED})) 1
		getSocketOption conn (CustomSockOpt (#{const SOL_SOCKET}, #{const SO_PASSCRED})) >>= print
		getPeerUCredRaw sock >>= print
		getPeerUCredPtr sock >>= \uc -> do
			print uc
--			peekPid uc >>= print
--			peekUid uc >>= print
--			peekGid uc >>= print
		getPeerUCredRaw conn >>= print
		getPeerUCredPtr conn >>= print
		getPeerUCred conn >>= print
		putStrLn $ "Connection from " ++ show peer
		void $ forkFinally (talk conn) (\_ -> close conn)
	talk conn = do
		msg <- recv conn 1024
		unless (BS.null msg) $ do
			sendAll conn msg
			talk conn

getPeerUCredRaw :: Socket -> IO Int
getPeerUCredRaw = (`getSocketOption` CustomSockOpt (#{const SOL_SOCKET}, #{const SO_PEERCRED}))

getPeerUCredPtr_, getPeerUCredPtr :: Socket -> IO (Ptr UCred)
getPeerUCredPtr_ = (intToPtr <$>) . (`getSocketOption` CustomSockOpt (#{const SOL_SOCKET}, #{const SO_PEERCRED}))
getPeerUCredPtr sock = getSocketOptionPtr sock #{const SOL_SOCKET} #{const SO_PEERCRED}

getPeerUCred :: Socket -> IO UCred
getPeerUCred sock = getSocketOption' sock #{const SOL_SOCKET} #{const SO_PEERCRED}

intToPtr :: Int -> Ptr a
intToPtr = intPtrToPtr . IntPtr

data UCred = UCred {
	ucredPid :: CInt,
	ucredUid :: CInt,
	ucredGid :: CInt
	} deriving Show

instance Storable UCred where
	sizeOf _ = #{size struct ucred}
	alignment _ = alignment (undefined :: CInt)
	poke p uc = do
		#{poke struct ucred, pid} p $ ucredPid uc
		#{poke struct ucred, uid} p $ ucredUid uc
		#{poke struct ucred, gid} p $ ucredGid uc
	peek p = UCred
		<$> #{peek struct ucred, pid} p
		<*> #{peek struct ucred, uid} p
		<*> #{peek struct ucred, gid} p

peekPid, peekUid, peekGid :: Ptr UCred -> IO CInt
peekPid = #{peek struct ucred, pid}
peekUid = #{peek struct ucred, uid}
peekGid = #{peek struct ucred, gid}

getSocketOption' :: Storable a => Socket -> CInt -> CInt -> IO a
getSocketOption' s l o = peek =<< getSocketOptionPtr s l o

getSocketOptionPtr :: forall a . Storable a => Socket -> CInt -> CInt -> IO (Ptr a)
getSocketOptionPtr s l o = alloca $ \ptr_v -> do
	let	sz = fromIntegral $ sizeOf (undefined :: a)
	withFdSocket s $ \fd -> with sz $ \ptr_sz -> do
		throwSocketErrorIfMinus1Retry_ "getSocketOptionPtr" $
			c_getsockopt fd l o ptr_v ptr_sz
		return ptr_v
