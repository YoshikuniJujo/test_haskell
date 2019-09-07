{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (
	stderr, File, fprintf_d, printf_,
	socket, FileDescriptor, Domain, Type, Protocol, afUnix, sockStream, protocol0,
	withSockaddrUn, SockaddrUn, sampleUnixDomainPath,
	pokeSunFamily, saFamilyTAfUnix, SunFamily, pokeSunPathString,
	connect, withSimpleMsghdr, sendmsg, msgFlags0, close,
	withMsghdrUcred, withMsghdrUcredServer, Ucred(..), Msghdr,

	c_peekMsgIov, c_peekMsgIovlen, c_peekIovBase, c_peekIovLen
	) where

import Control.Monad
import Data.Foldable
import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

import System.Posix.Types

import Base

#define _GNU_SOURCE

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include "hello.h"

foreign import ccall "fprintf"
	c_fprintf_d :: Ptr File -> CString -> CInt -> IO CInt
foreign import capi "value stderr" c_stderr :: Ptr File

newtype File = File (Ptr File) deriving Show

fprintf_d :: File -> String -> CInt -> IO CInt
fprintf_d (File fd) s n = withCString s $ \cs -> do
	r <-c_fprintf_d fd cs n
	r <$ when (r < 0) (error $ "c_fprintf_d: return error " ++ show r)

stderr :: File
stderr = File c_stderr

foreign import ccall "printf" c_printf_ :: CString -> IO CInt

printf_ :: String -> IO CInt
printf_ s = withCString s $ \cs -> do
	r <- c_printf_ cs
	r <$ when (r < 0) (error $ "c_printf_: return error " ++ show r)

foreign import ccall "socket" c_socket :: CInt -> CInt -> CInt -> IO CInt

c_AF_UNIX :: CInt
c_AF_UNIX = #{const AF_UNIX}

c_SOCK_STREAM :: CInt
c_SOCK_STREAM = #{const SOCK_STREAM}

newtype Domain = Domain CInt deriving Show

newtype Type = Type CInt deriving Show

newtype Protocol = Protocol CInt deriving Show

socket :: Domain -> Type -> Protocol -> IO FileDescriptor
socket (Domain d) (Type t) (Protocol p) = FileDescriptor <$> do
	r <- c_socket d t p
	(r <$) . when (r < 0) $ do
		error $	"c_socket: return error " ++ show r  ++ "\n" ++
			"errno: " ++ show c_errno ++ "\n"

afUnix :: Domain
afUnix = Domain c_AF_UNIX

sockStream :: Type
sockStream = Type c_SOCK_STREAM

protocol0 :: Protocol
protocol0 = Protocol 0

data SockaddrUn

withSockaddrUn :: (Ptr SockaddrUn -> IO a) -> IO a
withSockaddrUn act = allocaBytes #{size struct sockaddr_un} $ \ptr -> do
	c_memset ptr 0 #{size struct sockaddr_un}
	act ptr

data SunFamily = SunFamily #{type sa_family_t}

saFamilyTAfUnix :: SunFamily
saFamilyTAfUnix = SunFamily c_sa_family_t_AF_UNIX

pokeSunFamily :: Ptr SockaddrUn -> SunFamily -> IO ()
pokeSunFamily sau (SunFamily sf) = c_pokeSunFamily sau sf

c_sa_family_t_AF_UNIX :: #{type sa_family_t}
c_sa_family_t_AF_UNIX = #{const AF_UNIX}

c_pokeSunFamily :: Ptr SockaddrUn -> #{type sa_family_t} -> IO ()
c_pokeSunFamily = #{poke struct sockaddr_un, sun_family}

pokeSunPathString :: Ptr SockaddrUn -> FilePath -> IO ()
pokeSunPathString sau fp = withCString fp $ \cs -> c_pokeSunPathString sau cs

sampleUnixDomainPath :: FilePath
sampleUnixDomainPath = #const_str UNIXDOMAIN_PATH

c_ptrSunPath :: Ptr SockaddrUn -> CString
c_ptrSunPath = #{ptr struct sockaddr_un, sun_path}

c_pokeSunPathString :: Ptr SockaddrUn -> CString -> IO ()
c_pokeSunPathString sau cstr = () <$ c_strcpy (c_ptrSunPath sau) cstr

connect :: FileDescriptor -> Ptr SockaddrUn -> IO ()
connect (FileDescriptor fd) sau = do
	r <- c_connect fd sau #{size struct sockaddr_un}
	when (r < 0) . error
		$ "c_connect: return error " ++ show r ++ "\n" ++
			"errno: " ++ show c_errno

foreign import ccall "connect"
	c_connect :: CInt -> Ptr SockaddrUn -> CInt -> IO CInt

close :: FileDescriptor -> IO ()
close (FileDescriptor fd) = do
	r <- c_close fd
	when (r < 0) . error
		$ "c_close: return error " ++ show r ++ "\n" ++
			"errno: " ++ show c_errno

foreign import ccall "close" c_close :: CInt -> IO CInt

data {-# CTYPE "sys/socket.h" "struct msghdr" #-} Msghdr

withSimpleMsghdr :: [String] -> (Ptr Msghdr -> IO a) -> IO a
withSimpleMsghdr ss act = withMsghdr $ \msgh -> do
	c_pokeMsgName msgh (nullPtr, 0)
	withIovecFromStrings ss $ \iov -> do
		c_pokeMsgIov msgh iov
		c_pokeMsgIovlen msgh $ length ss
		c_pokeMsgControl msgh $ nullPtr
		c_pokeMsgControllen msgh 0
		act msgh

withMsghdr :: (Ptr Msghdr -> IO a) -> IO a
withMsghdr = allocaBytes #{size struct msghdr}

c_pokeMsgName :: Ptr Msghdr -> CStringLen -> IO ()
c_pokeMsgName mh (cs, l) = do
	#{poke struct msghdr, msg_name} mh cs
	#{poke struct msghdr, msg_namelen} mh l

c_pokeMsgIov :: Ptr Msghdr -> Ptr Iovec -> IO ()
c_pokeMsgIov = #{poke struct msghdr, msg_iov}

c_peekMsgIov :: Ptr Msghdr -> IO (Ptr Iovec)
c_peekMsgIov = #{peek struct msghdr, msg_iov}

c_pokeMsgIovlen :: Ptr Msghdr -> Int -> IO ()
c_pokeMsgIovlen = #{poke struct msghdr, msg_iovlen}

c_peekMsgIovlen :: Ptr Msghdr -> IO Int
c_peekMsgIovlen = #{peek struct msghdr, msg_iovlen}

c_pokeMsgControl :: Ptr Msghdr -> Ptr a -> IO ()
c_pokeMsgControl = #{poke struct msghdr, msg_control}

c_pokeMsgControllen :: Ptr Msghdr -> #{type size_t} -> IO ()
c_pokeMsgControllen = #{poke struct msghdr, msg_controllen}

data Iovec

withIovecFromStrings :: [String] -> (Ptr Iovec -> IO a) -> IO a
withIovecFromStrings ss act =
	withMultiCStringLens ss (`withIovecFromCStrings` act)

withIovecFromSizes :: [#type size_t] -> (Ptr Iovec -> IO a) -> IO a
withIovecFromSizes ss act =
	allocaMultiArrays (fromIntegral <$> ss) (`withIovecFromCStrings` act)

withIovecFromCStrings :: [CStringLen] -> (Ptr Iovec -> IO a) -> IO a
withIovecFromCStrings cstrs act = withIovec (length cstrs) $ \iov -> do
	for_ (zip (iterate nextIovecPtr iov) cstrs) $ \(i, (p, l)) ->
		c_pokeIovBase i p >> c_pokeIovLen i (fromIntegral l)
	act iov

withMultiCStringLens :: [String] -> ([CStringLen] -> IO a) -> IO a
withMultiCStringLens [] act = act []
withMultiCStringLens (s : ss) act =
	withCStringLen s $ \cs -> withMultiCStringLens ss $ act . (cs :)

allocaMultiArrays :: Storable a => [Int] -> ([(Ptr a, Int)] -> IO b) -> IO b
allocaMultiArrays [] act = act []
allocaMultiArrays (l : ls) act =
	allocaArray l $ \p -> allocaMultiArrays ls $ act . ((p, l) :)

withIovec :: Int -> (Ptr Iovec -> IO a) -> IO a
withIovec n = allocaBytes $ #{size struct iovec} * n

nextIovecPtr :: Ptr Iovec -> Ptr Iovec
nextIovecPtr = (`plusPtr` #{size struct iovec})

c_pokeIovBase :: Ptr Iovec -> Ptr a -> IO ()
c_pokeIovBase = #poke struct iovec, iov_base

c_peekIovBase :: Ptr Iovec -> IO (Ptr a)
c_peekIovBase = #peek struct iovec, iov_base

c_pokeIovLen :: Ptr Iovec -> #{type size_t} -> IO ()
c_pokeIovLen = #poke struct iovec, iov_len

c_peekIovLen :: Ptr Iovec -> IO #type size_t
c_peekIovLen = #peek struct iovec, iov_len

msgFlags0 :: MsgFlags
msgFlags0 = MsgFlags 0

sendmsg :: FileDescriptor -> Ptr Msghdr -> MsgFlags -> IO ()
sendmsg (FileDescriptor fd) mh (MsgFlags mf) = do
	r <- c_sendmsg fd mh mf
	when (r < 0) . error
		$ "c_sendmsg: return error " ++ show r ++ "\n" ++
			"errno: " ++ show c_errno

foreign import ccall "sendmsg" c_sendmsg :: CInt -> Ptr Msghdr -> CInt -> IO #type ssize_t

data CmsghdrUcred
data {-# CTYPE "sys/socket.h" "struct cmsghdr" #-} Cmsghdr

withCmsghdrUcred :: (Ptr CmsghdrUcred -> IO a) -> IO a
withCmsghdrUcred act =
	allocaBytes #{const CMSG_SPACE(sizeof(struct ucred))} $ \ptr -> do
		c_memset ptr 0 #const CMSG_SPACE(sizeof(struct ucred))
		act ptr

foreign import capi "sys/socket.h CMSG_FIRSTHDR" c_cmsg_firsthdr :: Ptr Msghdr -> Ptr Cmsghdr

c_pokeCmsgLen :: Ptr Cmsghdr -> #{type socklen_t} -> IO ()
c_pokeCmsgLen = #poke struct cmsghdr, cmsg_len

c_peekCmsgLen :: Ptr Cmsghdr -> IO #type socklen_t
c_peekCmsgLen = #peek struct cmsghdr, cmsg_len

foreign import capi "sys/socket.h CMSG_LEN" c_cmsg_len :: #{type socklen_t} -> #{type socklen_t}

c_pokeCmsgLevel :: Ptr Cmsghdr -> CInt -> IO ()
c_pokeCmsgLevel = #poke struct cmsghdr, cmsg_level

c_peekCmsgLevel :: Ptr Cmsghdr -> IO CInt
c_peekCmsgLevel = #peek struct cmsghdr, cmsg_level

c_SOL_SOCKET :: CInt
c_SOL_SOCKET = #const SOL_SOCKET

c_pokeCmsgType :: Ptr Cmsghdr -> CInt -> IO ()
c_pokeCmsgType = #poke struct cmsghdr, cmsg_type

c_peekCmsgType :: Ptr Cmsghdr -> IO CInt
c_peekCmsgType = #peek struct cmsghdr, cmsg_type

c_SCM_CREDENTIALS :: CInt
c_SCM_CREDENTIALS = #const SCM_CREDENTIALS

foreign import capi "sys/socket.h CMSG_DATA" c_cmsg_data :: Ptr Cmsghdr -> Ptr a

data Ucred = Ucred {
	ucredPid :: ProcessID,
	ucredUid :: UserID,
	ucredGid :: GroupID } deriving Show

{-
pokeUcredPid :: Ptr Ucred -> #{type pid_t} -> IO ()
pokeUcredPid = #poke struct ucred, pid

pokeUcredUid :: Ptr Ucred -> #{type uid_t} -> IO ()
pokeUcredUid = #poke struct ucred, uid

pokeUcredGid :: Ptr Ucred -> #{type gid_t} -> IO ()
pokeUcredGid = #poke struct ucred, gid
-}

instance Storable Ucred where
	sizeOf _ = #size struct ucred
	alignment _ = #alignment struct ucred
	peek p = Ucred
		<$> #{peek struct ucred, pid} p
		<*> #{peek struct ucred, uid} p
		<*> #{peek struct ucred, gid} p
	poke p uc = do
		#{poke struct ucred, pid} p $ ucredPid uc
		#{poke struct ucred, uid} p $ ucredUid uc
		#{poke struct ucred, gid} p $ ucredGid uc

withMsghdrUcred :: [String] -> Ucred -> (Ptr Msghdr -> IO a) -> IO a
withMsghdrUcred ss uc act = withMsghdr $ \msgh -> do
	c_pokeMsgName msgh (nullPtr, 0)
	withIovecFromStrings ss $ \iov -> do
		c_pokeMsgIov msgh iov
		c_pokeMsgIovlen msgh $ length ss
		withCmsghdrUcred $ \cmsg -> do
			c_pokeMsgControl msgh cmsg
			c_pokeMsgControllen msgh #const CMSG_SPACE(sizeof(struct ucred))
			let	cmsgp = c_cmsg_firsthdr msgh
			c_pokeCmsgLen cmsgp $ c_cmsg_len #size struct ucred
			c_pokeCmsgLevel cmsgp c_SOL_SOCKET
			c_pokeCmsgType cmsgp c_SCM_CREDENTIALS
			let	ucredp = c_cmsg_data cmsgp
			poke ucredp uc
			act msgh

withMsghdrUcredServer ::
	[#type size_t] -> (Ptr Msghdr -> IO a) -> (Ptr Msghdr -> Ptr Ucred -> IO b) -> IO b
withMsghdrUcredServer bs act1 act2 = withMsghdr $ \msgh -> do
	c_pokeMsgName msgh (nullPtr, 0)
	withIovecFromSizes bs $ \iov -> do
		c_pokeMsgIov msgh iov
		c_pokeMsgIovlen msgh $ length bs
		withCmsghdrUcred $ \cmsg -> do
			c_pokeMsgControl msgh cmsg
			c_pokeMsgControllen msgh #const CMSG_SPACE(sizeof(struct ucred))
			() <$ act1 msgh
			let	cmsgp = c_cmsg_firsthdr msgh
			when (cmsgp == nullPtr) $ error "bad"
			cml <- c_peekCmsgLen cmsgp
			when (cml /= c_cmsg_len #size struct ucred) $ error "bad"
			cmlvl <- c_peekCmsgLevel cmsgp
			when (cmlvl /= c_SOL_SOCKET) $ error "bad"
			cmt <- c_peekCmsgType cmsgp
			when (cmt /= c_SCM_CREDENTIALS) $ error "bad"
			let	ucredp = c_cmsg_data cmsgp
			act2 msgh ucredp

foreign import ccall "memset" c_memset :: Ptr a -> CInt -> CSize -> IO ()
foreign import ccall "strcpy" c_strcpy :: CString -> CString -> IO CString
