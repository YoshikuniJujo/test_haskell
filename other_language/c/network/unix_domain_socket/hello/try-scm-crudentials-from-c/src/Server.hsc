{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Server (
	bind, listen, poll, Pollfd(..), PollEvents, pollin,
	accept
	) where

import Control.Monad
import Data.List
import Data.Word
import Data.Int
import Data.Time.Clock
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.Types

import Lib
import Base

#include <sys/socket.h>
#include <sys/un.h>
#include <poll.h>

bind :: FileDescriptor -> Ptr SockaddrUn -> IO ()
bind (FileDescriptor fd) sau = do
	r <- c_bind fd sau #{size struct sockaddr_un}
	when (r < 0) . error $
		"c_bind: return error " ++ show r ++ "\n" ++
		"errno: " ++ show c_errno ++ "\n"

foreign import ccall "bind" c_bind :: CInt -> Ptr SockaddrUn -> #{type socklen_t} -> IO CInt

listen :: FileDescriptor -> CInt -> IO ()
listen (FileDescriptor fd) bl = do
	r <- c_listen fd bl
	when (r < 0) . error $
		"c_listen: return error " ++ show r ++ "\n" ++
		"errno: " ++ show c_errno ++ "\n"

foreign import ccall "listen" c_listen :: CInt -> CInt -> IO CInt

data Pollfd = Pollfd {
	pollfdFileDescriptor :: FileDescriptor,
	pollfdEvents :: PollEvents,
	pollfdRevents :: Maybe PollEvents } deriving Show

newtype PollEvents = PollEvents #{type short} deriving (Show, Storable, Eq)

pollin :: PollEvents
pollin = PollEvents #const POLLIN

instance Storable Pollfd where
	sizeOf _ = #size struct pollfd
	alignment _ = #alignment struct pollfd
	peek p = Pollfd
		<$> #{peek struct pollfd, fd} p
		<*> #{peek struct pollfd, events} p
		<*> (Just <$> #{peek struct pollfd, revents} p)
	poke p pe = do
		#{poke struct pollfd, fd} p $ pollfdFileDescriptor pe
		#{poke struct pollfd, events} p $ pollfdEvents pe
		case pollfdRevents pe of
			Just re -> #{poke struct pollfd, revents} p re
			Nothing -> return ()

poll :: [Pollfd] -> Maybe NominalDiffTime -> IO (CInt, [Pollfd])
poll pfds to = allocaArray ln $ \pfdsp -> do
	pokeArray pfdsp pfds
	n <- c_poll pfdsp ln $ toMillisecond to
	when (n < 0) . error $
		"c_poll: return error " ++ show n ++ "\n" ++
		"errno: " ++ show c_errno ++ "\n"
	opfds <- peekArray ln pfdsp
	return (n, opfds)
	where
	ln :: Integral n => n
	ln = genericLength pfds

toMillisecond :: Maybe NominalDiffTime -> CInt
toMillisecond Nothing = - 1
-- toMillisecond (Just ndt) = truncate $ 1000 * nominalDiffTimeToSeconds ndt
toMillisecond (Just ndt) = truncate $ 1000 * ndt

foreign import ccall "poll" c_poll :: Ptr Pollfd -> #{type nfds_t} -> CInt -> IO CInt

accept :: FileDescriptor -> Ptr SockaddrUn -> IO FileDescriptor
accept (FileDescriptor fd) sau = alloca $ \lnp -> do
	poke lnp #size struct sockaddr_un
	cfd <- c_accept fd sau lnp
	when (fd < 0) . error $
		"c_accept: return error " ++ show fd ++ "\n" ++
		"errno: " ++ show c_errno ++ "\n"
	oln <- peek lnp
	when (oln /= #size struct sockaddr_un) . putStrLn $
		"c_accept: addrlen /= sizeof(struct sockaddr_un)\n" ++
		"sizeof(struct sockaddr_un): " ++ show (#{size struct sockaddr_un} :: CInt) ++ "\n" ++
		"oln: " ++ show oln ++ "\n"
	{-
	when (oln /= #size struct sockaddr_un) . error $
		"c_accept: addrlen /= sizeof(struct sockaddr_un): " ++ show oln
		-}
	return $ FileDescriptor cfd

foreign import ccall "accept" c_accept :: CInt -> Ptr SockaddrUn -> Ptr #{type socklen_t} ->  IO CInt
